module Warpless.HTTP1
  ( http1,
  )
where

import Control.Concurrent qualified as Conc (yield)
import Control.Exception (SomeAsyncException (..), SomeException, catch, fromException, throwIO)
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
import Warpless.Request (recvRequest)
import Warpless.Response (sendResponse)
import Warpless.Settings (Settings (settingsOnException), defaultOnExceptionResponse)
import Warpless.Source (Source, leftoverSource, mkSource, readSource)
import Warpless.Types (WeirdClient)

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

http1server :: Settings -> IO GMTDate -> Connection -> Application -> SockAddr -> IORef Bool -> Source -> IO ()
http1server settings getDate conn app addr istatus src =
  loop `catch` \(exception :: SomeException) ->
    if
      | Just _ <- fromException @SomeAsyncException exception -> throwIO exception
      | Just _ <- fromException @WeirdClient exception -> pure ()
      | otherwise -> do
          _ <- sendErrorResponse getDate conn istatus defaultRequest {remoteHost = addr} exception
          throwIO exception
  where
    loop :: IO ()
    loop = do
      (request, idxhdr, nextBodyFlush) <- recvRequest settings conn addr src
      keepAlive <-
        processRequest settings getDate conn app istatus src request idxhdr nextBodyFlush
          `UnliftIO.catchAny` \e -> do
            settingsOnException settings (Just request) e
            -- Don't throw the error again to prevent calling settingsOnException twice.
            pure False
      when keepAlive loop

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
processRequest settings getDate conn app istatus source request idxhdr nextBodyFlush = do
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
        writeIORef istatus False
        keepAlive <- sendResponse conn getDate request idxhdr (readSource source) response
        writeIORef keepAliveRef keepAlive
        pure ResponseReceived

  case r of
    Right ResponseReceived -> pure ()
    Left (e :: SomeException) -> do
      keepAlive <- sendErrorResponse getDate conn istatus request e
      settingsOnException settings (Just request) e
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
sendErrorResponse getDate conn istatus req exception = do
  status <- readIORef istatus
  if shouldSendErrorResponse && status
    then sendResponse conn getDate req defaultIndexRequestHeader (return ByteString.empty) defaultOnExceptionResponse
    else return False
  where
    shouldSendErrorResponse
      | Just _ <- fromException @WeirdClient exception = False
      | otherwise = True

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody source =
  loop
  where
    loop = do
      bytes <- source
      when (not (ByteString.null bytes)) loop
