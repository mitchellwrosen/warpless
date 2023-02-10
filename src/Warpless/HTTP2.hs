{-# LANGUAGE RecordWildCards #-}

module Warpless.HTTP2
  ( http2,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IORef qualified as I
import Network.HTTP2.Server qualified as H2
import Network.Socket (SockAddr)
import Network.Socket.BufferPool
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import System.TimeManager qualified as TimeManager
import UnliftIO qualified
import Warpless.HTTP2.File
import Warpless.HTTP2.PushPromise
import Warpless.HTTP2.Request
import Warpless.HTTP2.Response
import Warpless.Settings qualified as S
import Warpless.Types

----------------------------------------------------------------

http2 :: S.Settings -> InternalInfo -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http2 settings ii conn app origAddr bs = do
  istatus <- newIORef False
  rawRecvN <- makeRecvN bs $ connRecv conn
  writeBuffer <- readIORef $ connWriteBuffer conn
  UnliftIO.bracket (TimeManager.initialize 30_000_000) TimeManager.stopManager \tm -> do
    -- This thread becomes the sender in http2 library.
    -- In the case of event source, one request comes and one
    -- worker gets busy. But it is likely that the receiver does
    -- not receive any data at all while the sender is sending
    -- output data from the worker. It's not good enough to tickle
    -- the time handler in the receiver only. So, we should tickle
    -- the time handler in both the receiver and the sender.
    let recvN = wrappedRecvN istatus rawRecvN
        conf =
          H2.Config
            { confWriteBuffer = bufBuffer writeBuffer,
              confBufferSize = bufSize writeBuffer,
              confSendAll = connSendAll conn,
              confReadN = recvN,
              confPositionReadMaker = pReadMaker ii,
              confTimeoutManager = tm
            }
    setConnHTTP2 conn True
    H2.run conf $ http2server settings ii origAddr app

-- | Converting WAI application to the server type of http2 library.
--
-- Since 3.3.11
http2server ::
  S.Settings ->
  InternalInfo ->
  SockAddr ->
  Application ->
  H2.Server
http2server settings ii addr app h2req0 _aux0 response = do
  req <- toWAIRequest h2req0
  ref <- I.newIORef Nothing
  eResponseReceived <- UnliftIO.tryAny $
    app req $ \rsp -> do
      (h2rsp, st, hasBody) <- fromResponse settings ii req rsp
      pps <- if hasBody then fromPushPromises ii req else return []
      I.writeIORef ref $ Just (h2rsp, pps, st)
      _ <- response h2rsp pps
      return ResponseReceived
  case eResponseReceived of
    Right ResponseReceived -> do
      Just (h2rsp, pps, st) <- I.readIORef ref
      let msiz = H2.responseBodySize h2rsp
      logResponse req st (fromIntegral @Int @Integer <$> msiz)
      mapM_ (logPushPromise req) pps
    Left e -> do
      S.settingsOnException settings (Just req) e
      let ersp = S.settingsOnExceptionResponse settings e
          st = responseStatus ersp
      (h2rsp', _, _) <- fromResponse settings ii req ersp
      let msiz = H2.responseBodySize h2rsp'
      _ <- response h2rsp' []
      logResponse req st (fromIntegral @Int @Integer <$> msiz)
  return ()
  where
    toWAIRequest h2req = toRequest ii settings addr hdr bdylen bdy
      where
        !hdr = H2.requestHeaders h2req
        !bdy = H2.getRequestBodyChunk h2req
        !bdylen = H2.requestBodySize h2req

    logResponse = S.settingsLogger settings

    logPushPromise req pp = logger req path (fromIntegral @Int @Integer siz)
      where
        !logger = S.settingsServerPushLogger settings
        !path = H2.promiseRequestPath pp
        !siz = case H2.responseBodySize $ H2.promiseResponse pp of
          Nothing -> 0
          Just s -> s

wrappedRecvN :: IORef Bool -> (BufSize -> IO ByteString) -> (BufSize -> IO ByteString)
wrappedRecvN istatus readN bufsize = do
  bs <- UnliftIO.handleAny handler $ readN bufsize
  when (not (BS.null bs)) (writeIORef istatus True)
  return bs
  where
    handler :: UnliftIO.SomeException -> IO ByteString
    handler _ = return ""
