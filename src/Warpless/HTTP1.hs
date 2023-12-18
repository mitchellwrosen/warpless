module Warpless.HTTP1
  ( http1,
  )
where

import Control.Concurrent qualified as Conc (yield)
import Control.Exception (SomeException, catch, fromException, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request (remoteHost), defaultRequest)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import UnliftIO qualified
import Warpless.Connection (Connection (..), connRecv)
import Warpless.Date (GMTDate)
import Warpless.Header (IndexedHeader, defaultIndexRequestHeader)
import Warpless.Request (NoKeepAliveRequest (NoKeepAliveRequest), recvRequest)
import Warpless.Response (sendResponse)
import Warpless.Settings (Settings (settingsOnException, settingsOnExceptionResponse))
import Warpless.Source (Source, leftoverSource, mkSource, readSource)
import Warpless.Types (ExceptionInsideResponseBody (ExceptionInsideResponseBody), InvalidRequest (BadFirstLine, ConnectionClosedByPeer))

http1 :: Settings -> IO GMTDate -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http1 settings getDate conn app addr bs0 = do
  istatus <- newIORef True
  source <-
    mkSource do
      bytes <- connRecv conn
      when (not (ByteString.null bytes)) (writeIORef istatus True)
      pure bytes
  leftoverSource source bs0
  http1server settings getDate conn app addr istatus source

http1server ::
  Settings ->
  IO GMTDate ->
  Connection ->
  Application ->
  SockAddr ->
  IORef Bool ->
  Source ->
  IO ()
http1server settings getDate conn app addr istatus src =
  loop True `catch` \(exception :: SomeException) ->
    case fromException @NoKeepAliveRequest exception of
      -- See comment below referencing
      -- https://github.com/yesodweb/wai/issues/618
      Just NoKeepAliveRequest -> pure ()
      Nothing ->
        case fromException @InvalidRequest exception of
          -- No valid request
          Just (BadFirstLine _) -> pure ()
          _ -> do
            _ <- sendErrorResponse settings getDate conn istatus defaultRequest {remoteHost = addr} exception
            throwIO exception
  where
    loop :: Bool -> IO ()
    loop firstRequest = do
      (request, idxhdr, nextBodyFlush) <- recvRequest firstRequest settings conn addr src
      keepAlive <-
        processRequest settings getDate conn app istatus src request idxhdr nextBodyFlush
          `UnliftIO.catchAny` \e -> do
            settingsOnException settings (Just request) e
            -- Don't throw the error again to prevent calling settingsOnException twice.
            pure False

      -- When doing a keep-alive connection, the other side may just
      -- close the connection. We don't want to treat that as an
      -- exceptional situation, so we pass in False to http1 (which
      -- in turn passes in False to recvRequest), indicating that
      -- this is not the first request. If, when trying to read the
      -- request headers, no data is available, recvRequest will
      -- throw a NoKeepAliveRequest exception, which we catch here
      -- and ignore. See: https://github.com/yesodweb/wai/issues/618

      when keepAlive (loop False)

processRequest ::
  Settings ->
  IO GMTDate ->
  Connection ->
  Application ->
  IORef Bool ->
  Source ->
  Request ->
  IndexedHeader ->
  IO ByteString ->
  IO Bool
processRequest settings getDate conn app istatus src req idxhdr nextBodyFlush = do
  -- In the event that some scarce resource was acquired during
  -- creating the request, we need to make sure that we don't get
  -- an async exception before calling the ResponseSource.
  keepAliveRef <- newIORef $ error "keepAliveRef not filled"

  r <-
    UnliftIO.tryAny $
      app req \res -> do
        -- FIXME consider forcing evaluation of the res here to
        -- send more meaningful error messages to the user.
        -- However, it may affect performance.
        writeIORef istatus False
        keepAlive <- sendResponse settings conn getDate req idxhdr (readSource src) res
        writeIORef keepAliveRef keepAlive
        return ResponseReceived

  case r of
    Right ResponseReceived -> return ()
    Left (e :: SomeException)
      | Just (ExceptionInsideResponseBody e') <- fromException e -> throwIO e'
      | otherwise -> do
          keepAlive <- sendErrorResponse settings getDate conn istatus req e
          settingsOnException settings (Just req) e
          writeIORef keepAliveRef keepAlive

  keepAlive <- readIORef keepAliveRef

  -- We just send a Response and it takes a time to
  -- receive a Request again. If we immediately call recv,
  -- it is likely to fail and cause the IO manager to do some work.
  -- It is very costly, so we yield to another Haskell
  -- thread hoping that the next Request will arrive
  -- when this Haskell thread will be re-scheduled.
  -- This improves performance at least when
  -- the number of cores is small.
  Conc.yield

  if keepAlive
    then do
      flushEntireBody nextBodyFlush
      pure True
    else pure False

sendErrorResponse :: Settings -> IO GMTDate -> Connection -> IORef Bool -> Request -> SomeException -> IO Bool
sendErrorResponse settings getDate conn istatus req e = do
  status <- readIORef istatus
  if shouldSendErrorResponse e && status
    then sendResponse settings conn getDate req defaultIndexRequestHeader (return ByteString.empty) errorResponse
    else return False
  where
    shouldSendErrorResponse se
      | Just ConnectionClosedByPeer <- fromException se = False
      | otherwise = True
    errorResponse = settingsOnExceptionResponse settings e

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody source =
  loop
  where
    loop = do
      bytes <- source
      when (not (ByteString.null bytes)) loop
