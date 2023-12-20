module Warpless.HTTP1
  ( http1,
  )
where

import Control.Concurrent qualified as Conc (yield)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import UnliftIO qualified
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.Date (GMTDate)
import Warpless.HTTP1.Request (receiveRequest)
import Warpless.Response (sendResponse)
import Warpless.Settings (Settings (settingsOnException), defaultOnExceptionResponse)
import Warpless.Source (Source, leftoverSource, mkSource, readSource)
import Warpless.Types (WeirdClient)

http1 :: Settings -> IO GMTDate -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http1 settings getDate conn app addr bytes0 = do
  -- Keep track of whether we "should respond" to the client at all, at any given time:
  --
  --   - Whenever we receive any bytes on the client connection, we set this to true.
  --   - Whenever we send a response to the client, we set this to false.
  --
  -- (Remember, one client connection can be reused for more than one request). Thus, whenever we hit an exception and
  -- end up asking ourselves, "should I tell the client about this (by way of 500 Internal Server Error)?", we query
  -- this boolean.
  shouldRespondRef <- newIORef True

  -- Create a source of bytes from the client's connection that sets `shouldRespond` per the description above.
  source <-
    mkSource do
      bytes <- Connection.receive conn
      when (not (ByteString.null bytes)) (writeIORef shouldRespondRef True)
      pure bytes

  -- We already read one chunk of bytes to determine if this was an HTTP2 request. It wasn't. Put those bytes back at
  -- the front of the source, for us to read next.
  leftoverSource source bytes0

  let loop :: IO ()
      loop = do
        try @WeirdClient (receiveRequest settings conn addr source) >>= \case
          -- A "weird client" exception means the client did something weird or wrong, like sent us some garbage
          -- request, or closed its end of the connection. We just ignore those, and do not dignify the client with a
          -- response. Again, it may have closed its connection, anyway.
          Left _ -> pure ()
          -- We got a request!
          Right (request, commonHeaders, nextBodyFlush) -> do
            keepAlive <-
              processRequest settings getDate conn app shouldRespondRef source request commonHeaders nextBodyFlush
                `catch` \exception ->
                  case exception of
                    AnyAsyncException -> throwIO exception
                    AnyWeirdClient -> pure False
                    _ -> do
                      settingsOnException settings (Just request) exception
                      -- Don't throw the error again to prevent calling settingsOnException twice.
                      pure False
            when keepAlive loop
  loop

processRequest ::
  Settings ->
  IO GMTDate ->
  Connection ->
  Application ->
  IORef Bool ->
  Source ->
  Request ->
  CommonRequestHeaders ->
  IO ByteString ->
  IO Bool
processRequest settings getDate conn app shouldRespondRef source request idxhdr nextBodyFlush = do
  -- In the event that some scarce resource was acquired during
  -- creating the request, we need to make sure that we don't get
  -- an async exception before calling the ResponseSource.
  keepAliveRef <- newIORef $ error "keepAliveRef not filled"

  r <-
    UnliftIO.tryAny $
      app request \response -> do
        -- FIXME consider forcing evaluation of the res here to
        -- send more meaningful error messages to the user.
        -- However, it may affect performance.
        writeIORef shouldRespondRef False
        keepAlive <- sendResponse conn getDate request idxhdr (readSource source) response
        writeIORef keepAliveRef keepAlive
        pure ResponseReceived

  case r of
    Right ResponseReceived -> pure ()
    Left exception -> do
      keepAlive <- sendErrorResponse getDate conn shouldRespondRef request exception
      settingsOnException settings (Just request) exception
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

  when keepAlive (flushEntireBody nextBodyFlush)
  pure keepAlive

sendErrorResponse :: IO GMTDate -> Connection -> IORef Bool -> Request -> SomeException -> IO Bool
sendErrorResponse getDate conn shouldRespondRef req exception = do
  shouldRespond <- readIORef shouldRespondRef
  if shouldSendErrorResponse && shouldRespond
    then sendResponse conn getDate req CommonRequestHeaders.empty (pure ByteString.empty) defaultOnExceptionResponse
    else pure False
  where
    shouldSendErrorResponse =
      case exception of
        AnyWeirdClient -> False
        _ -> True

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody source =
  loop
  where
    loop = do
      bytes <- source
      when (not (ByteString.null bytes)) loop

pattern AnyAsyncException :: SomeException
pattern AnyAsyncException <- (fromException @SomeAsyncException -> Just _)

pattern AnyWeirdClient :: SomeException
pattern AnyWeirdClient <- (fromException @WeirdClient -> Just _)
