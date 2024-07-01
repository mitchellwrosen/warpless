module Warpless.Run
  ( run,
  )
where

import Control.Concurrent (yield)
import Control.Exception (MaskingState (..), allowInterrupt)
import Control.Monad (forever)
import Data.ByteString qualified as ByteString
import Data.Vault.Lazy qualified as Vault
import GHC.IO (unsafeUnmask)
import GHC.Real (fromIntegral)
import Ki qualified
import Network.HTTP.Date (epochTimeToHTTPDate, formatHTTPDate)
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Network
import Network.Wai (RequestBodyLength (ChunkedBody, KnownLength))
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai (Request (..))
import System.Posix.Time (epochTime)
import UnliftIO qualified
import Warpless.ByteString qualified as ByteString
import Warpless.Cached (cached)
import Warpless.ChunkedSource qualified as ChunkedSource
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.Connection (Connection (..))
import Warpless.Connection qualified as Connection
import Warpless.Date (GMTDate)
import Warpless.Exception (ignoringExceptions)
import Warpless.Prelude
import Warpless.Request (receiveRequestHeaders)
import Warpless.Response (sendResponse)
import Warpless.Settings (Settings (settingsOnException), defaultOnExceptionResponse)
import Warpless.Source qualified as Source
import Warpless.SourceN qualified as SourceN
import Warpless.Types (WeirdClient (WeirdClient))

run :: Settings -> Network.Socket -> (Wai.Request -> (Wai.Response -> IO ()) -> IO ()) -> IO v
run settings serverSocket application = do
  Ki.scoped \scope -> do
    -- Create an IO action that gets the current date (formatted as a ByteString like "Wed, 20 Dec 2023 03:29:48 GMT"),
    -- suitable as the value of a Date header. It is computed on-demand, at most once per second.
    --
    -- Quick-and-dirty benchmarking shows that it takes about 130 nanoseconds to make the syscall, but only 3
    -- nanoseconds to grab it out of this cache.
    getDate <- cached scope (formatHTTPDate . epochTimeToHTTPDate <$> epochTime) 1_000_000

    -- Mask asynchronous exceptions so we don't accept a client socket and fail to close it due to an intervening
    -- asynchronous exception.
    mask_ do
      -- Accept clients in a loop, forever.
      --
      -- With asynchronous exceptions masked, we accept a client. We then fork a client handler on a thread with
      -- asynchronous exceptions uninterruptibly masked, and that thread installs an exception handler to close the client
      -- socket before it allows the delivery of asynchronous exceptions. Thus, every client socket we open is ultimately
      -- closed.
      forever do
        -- The main client `accept` is interruptible, so even with asynchronous exceptions (interruptibly) masked, if
        -- `accept` blocks, any pending asynchronous exception will be delivered.
        --
        -- However, just to be safe, we explicitly poll for asynchronous exceptions once per client accepted. This means
        -- that even in a hot accept-loop that never blocks, we'll still promptly receive asynchronous exceptions.
        allowInterrupt

        -- Accept a client!
        (clientSocket, clientSockAddr) <- Network.accept serverSocket

        -- Fork a thread to handle the client, with asynchronous exceptions uninterruptibly masked. The client thread is
        -- meant to arrange so that by the time asynchronous exceptions are allowed again, an exception handler is
        -- installed to close the client socket.
        --
        -- The one exception we allow to propagate all the way up here is the distinguished WeirdClient exception,
        -- which means the client did something wrong or weird, not us. We don't want that to bring down the server.
        -- So, we catch it here (so that it does not propagate to the main thread) and ignore it. Badly-written clients
        -- that may *think* they are speaking a dialogue of HTTP we can understand will just be ignored.
        void do
          Ki.forkWith @() scope clientThreadOptions do
            withClient clientSocket clientSockAddr \client ->
              handleClient settings application getDate client `catch` \WeirdClient -> pure ()
  where
    clientThreadOptions :: Ki.ThreadOptions
    clientThreadOptions =
      Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible}

withClient :: Network.Socket -> Network.SockAddr -> (Connection -> IO ()) -> IO ()
withClient socket sockAddr action = do
  -- Disable Nagle's algorithm, which attempts to reduce network traffic by buffering writes until enough data is
  -- enqueued. Ignore exceptions, because on a unix socket, this operation throws an exception.
  ignoringExceptions (Network.setSocketOption socket Network.NoDelay 1)

  -- Wrap the socket in a connection object. This never throws an exception.
  conn <- Connection.create socket sockAddr

  result <- try @SomeException (unsafeUnmask (action conn))

  Connection.close conn

  case result of
    Left exception -> do
      case fromException @WeirdClient exception of
        Nothing -> throwIO exception
        Just _ -> pure ()
    Right _ -> pure ()

receiveTheRequest :: Connection -> IO (Wai.Request, CommonRequestHeaders)
receiveTheRequest conn = do
  (requestMethod, rawPathInfo, rawQueryString, httpVersion, requestHeaders) <- receiveRequestHeaders conn.source

  let commonRequestHeaders = CommonRequestHeaders.make requestHeaders

  (getBodyChunk, bodyLength) <-
    if CommonRequestHeaders.hasChunkedTransferEncoding commonRequestHeaders
      then do
        chunkedSource <- ChunkedSource.make conn.source
        pure (ChunkedSource.read chunkedSource, ChunkedBody)
      else do
        let len = maybe 0 (fromIntegral @Int64 @Int . ByteString.readInt64) (CommonRequestHeaders.getContentLength commonRequestHeaders)
        sourceN <- SourceN.new conn.source len
        pure (SourceN.read sourceN, KnownLength (fromIntegral @Int @Word64 len))

  -- Eagerly reply to "Expect: 100-continue" header, if it exists.
  when (CommonRequestHeaders.hasExpect100Continue commonRequestHeaders) do
    Connection.send
      conn
      if httpVersion == Http.http11
        then "HTTP/1.1 100 Continue\r\n\r\n"
        else "HTTP/1.0 100 Continue\r\n\r\n"
    yield

  let request =
        Wai.Request
          requestMethod
          httpVersion
          rawPathInfo
          rawQueryString
          requestHeaders
          False -- isSecure
          conn.sockAddr
          (Http.decodePathSegments rawPathInfo) -- pathInfo
          (Http.parseQuery rawQueryString) -- queryString
          getBodyChunk -- requestBody
          Vault.empty -- vault
          bodyLength -- requestBodyLength
          (CommonRequestHeaders.getHost commonRequestHeaders) -- requestHeaderHost
          (CommonRequestHeaders.getRange commonRequestHeaders) -- requestHeaderRange
          (CommonRequestHeaders.getReferer commonRequestHeaders) -- requestHeaderReferer
          (CommonRequestHeaders.getUserAgent commonRequestHeaders) -- requestHeaderUserAgent
  pure (request, commonRequestHeaders)

sendTheResponse ::
  Settings ->
  IO GMTDate ->
  ((Wai.Response -> IO ()) -> IO ()) ->
  Wai.Request ->
  CommonRequestHeaders ->
  Connection ->
  IO Bool
sendTheResponse settings getDate application request commonRequestHeaders conn = do
  keepAliveRef <- newIORef False

  result <-
    UnliftIO.tryAny do
      application \response -> do
        keepAlive <- sendResponse conn getDate request commonRequestHeaders (Source.read conn.source) response
        writeIORef keepAliveRef keepAlive

  keepAlive <-
    case result of
      Right () -> readIORef keepAliveRef
      Left exception -> do
        settings.settingsOnException exception
        void $
          sendResponse
            conn
            getDate
            request
            CommonRequestHeaders.empty
            (pure ByteString.empty)
            defaultOnExceptionResponse
        pure False

  -- If we are reusing this connection for another request, then first flush away all of the request body that (for
  -- whatever reason) was not read by the application).
  when keepAlive do
    doWhile do
      bytes <- Wai.getRequestBodyChunk request
      pure (not (ByteString.null bytes))

  -- We just send a Response and it takes a time to
  -- receive a Request again. If we immediately call recv,
  -- it is likely to fail and cause the IO manager to do some work.
  -- It is very costly, so we yield to another Haskell
  -- thread hoping that the next Request will arrive
  -- when this Haskell thread will be re-scheduled.
  -- This improves performance at least when
  -- the number of cores is small.
  yield

  pure keepAlive

handleClient ::
  Settings ->
  (Wai.Request -> (Wai.Response -> IO ()) -> IO ()) ->
  IO GMTDate ->
  Connection ->
  IO ()
handleClient settings application getDate conn = do
  doWhile do
    (request, commonRequestHeaders) <- receiveTheRequest conn
    sendTheResponse settings getDate (application request) request commonRequestHeaders conn
