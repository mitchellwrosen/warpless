{-# LANGUAGE RecordWildCards #-}

module Warpless.HTTP2
  ( http2,
  )
where

import Data.IORef qualified as I
import Network.HTTP2.Server qualified as H2
import Network.Socket (SockAddr)
import Network.Socket.BufferPool (makeRecvN)
import Network.Wai (Application)
import Network.Wai.Internal (ResponseReceived (..))
import System.TimeManager qualified as TimeManager
import UnliftIO qualified
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.Date (GMTDate)
import Warpless.HTTP2.File (pReadMaker)
import Warpless.HTTP2.PushPromise (fromPushPromises)
import Warpless.HTTP2.Request (toRequest)
import Warpless.HTTP2.Response (fromResponse)
import Warpless.Prelude
import Warpless.Settings qualified as S
import Warpless.WriteBuffer (WriteBuffer (..))

http2 :: S.Settings -> IO GMTDate -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http2 settings getDate conn app peerAddr bs = do
  recvN <- makeRecvN bs (Connection.receive conn)
  writeBuffer <- readIORef (Connection.writeBufferRef conn)
  UnliftIO.bracket (TimeManager.initialize 30_000_000) TimeManager.stopManager \tm -> do
    let conf =
          H2.Config
            { confWriteBuffer = bufBuffer writeBuffer,
              confBufferSize = bufSize writeBuffer,
              confSendAll = Connection.send conn,
              confReadN = recvN,
              confPositionReadMaker = pReadMaker,
              confTimeoutManager = tm,
              confMySockAddr = Connection.socketAddr conn,
              confPeerSockAddr = peerAddr
            }
    Connection.setIsHttp2 conn
    H2.run H2.defaultServerConfig conf $ http2server settings getDate peerAddr app

-- | Converting WAI application to the server type of http2 library.
--
-- Since 3.3.11
http2server :: S.Settings -> IO GMTDate -> SockAddr -> Application -> H2.Server
http2server settings getDate addr app h2req0 _aux0 response = do
  req <- toWAIRequest h2req0
  ref <- I.newIORef Nothing
  eResponseReceived <-
    UnliftIO.tryAny
      $ app req \rsp -> do
        (h2rsp, hasBody) <- fromResponse getDate req rsp
        pps <- if hasBody then fromPushPromises req else pure []
        I.writeIORef ref (Just pps)
        _ <- response h2rsp pps
        pure ResponseReceived
  case eResponseReceived of
    Right ResponseReceived -> do
      Just pps <- I.readIORef ref
      traverse_ (logPushPromise req) pps
    Left e -> do
      S.settingsOnException settings e
      let ersp = S.defaultOnExceptionResponse
      (h2rsp', _) <- fromResponse getDate req ersp
      response h2rsp' []
  where
    toWAIRequest h2req = toRequest settings addr hdr bdylen bdy
      where
        !hdr = H2.requestHeaders h2req
        !bdy = H2.getRequestBodyChunk h2req
        !bdylen = H2.requestBodySize h2req

    logPushPromise req pp = logger req path (from @Int @Integer siz)
      where
        !path = H2.promiseRequestPath pp
        !siz = fromMaybe 0 (H2.responseBodySize (H2.promiseResponse pp))
    logger = S.settingsServerPushLogger settings
