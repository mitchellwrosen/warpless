module Warpless.HTTP2.PushPromise
  ( fromPushPromises,
  )
where

import GHC.Real (fromIntegral)
import Network.HTTP.Types qualified as H
import Network.HTTP2.Server qualified as H2
import Network.Wai (Request)
import Warpless.FileInfo (FileInfo (..), getFileInfo)
import Warpless.HTTP2.Request (getHTTP2Data)
import Warpless.HTTP2.Types (HTTP2Data (http2dataPushPromise), PushPromise (PushPromise))
import Warpless.Prelude

fromPushPromises :: Request -> IO [H2.PushPromise]
fromPushPromises req = do
  mh2data <- getHTTP2Data req
  let pp = case mh2data of
        Nothing -> []
        Just h2data -> http2dataPushPromise h2data
  catMaybes <$> traverse fromPushPromise pp

fromPushPromise :: PushPromise -> IO (Maybe H2.PushPromise)
fromPushPromise (PushPromise path file rsphdr w) = do
  efinfo <- try @IOException $ getFileInfo file
  case efinfo of
    Left _exception -> pure Nothing
    Right finfo -> do
      let !siz = fileInfoSize finfo
          !fileSpec = H2.FileSpec file 0 (fromIntegral @Integer @H2.ByteCount siz)
          !rsp = H2.responseFile H.ok200 rsphdr fileSpec
          !pp = H2.pushPromise path rsp w
      pure $ Just pp
