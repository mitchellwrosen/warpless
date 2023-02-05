module Warpless.HTTP2.PushPromise
  ( fromPushPromises,
  )
where

import Network.HTTP.Types qualified as H
import Network.HTTP2.Server qualified as H2
import Network.Wai
import UnliftIO qualified
import Warpless.FileInfoCache
import Warpless.HTTP2.Request (getHTTP2Data)
import Warpless.HTTP2.Types
import Warpless.Imports
import Warpless.Types

fromPushPromises :: InternalInfo -> Request -> IO [H2.PushPromise]
fromPushPromises ii req = do
  mh2data <- getHTTP2Data req
  let pp = case mh2data of
        Nothing -> []
        Just h2data -> http2dataPushPromise h2data
  catMaybes <$> mapM (fromPushPromise ii) pp

fromPushPromise :: InternalInfo -> PushPromise -> IO (Maybe H2.PushPromise)
fromPushPromise ii (PushPromise path file rsphdr w) = do
  efinfo <- UnliftIO.tryIO $ getFileInfo ii file
  case efinfo of
    Left (_ex :: UnliftIO.IOException) -> return Nothing
    Right finfo -> do
      let !siz = fileInfoSize finfo
          !fileSpec = H2.FileSpec file 0 (fromIntegral @Integer @H2.ByteCount siz)
          !rsp = H2.responseFile H.ok200 rsphdr fileSpec
          !pp = H2.pushPromise path rsp w
      return $ Just pp
