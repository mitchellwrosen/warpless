module Warpless.HTTP2.PushPromise
  ( fromPushPromises,
  )
where

import Data.Maybe (catMaybes)
import Network.HTTP.Types qualified as H
import Network.HTTP2.Server qualified as H2
import Network.Wai
import UnliftIO qualified
import Warpless.FileInfo (FileInfo (..), getFileInfo)
import Warpless.HTTP2.Request (getHTTP2Data)
import Warpless.HTTP2.Types

fromPushPromises :: Request -> IO [H2.PushPromise]
fromPushPromises req = do
  mh2data <- getHTTP2Data req
  let pp = case mh2data of
        Nothing -> []
        Just h2data -> http2dataPushPromise h2data
  catMaybes <$> mapM fromPushPromise pp

fromPushPromise :: PushPromise -> IO (Maybe H2.PushPromise)
fromPushPromise (PushPromise path file rsphdr w) = do
  efinfo <- UnliftIO.tryIO $ getFileInfo file
  case efinfo of
    Left (_ex :: UnliftIO.IOException) -> return Nothing
    Right finfo -> do
      let !siz = fileInfoSize finfo
          !fileSpec = H2.FileSpec file 0 (fromIntegral @Integer @H2.ByteCount siz)
          !rsp = H2.responseFile H.ok200 rsphdr fileSpec
          !pp = H2.pushPromise path rsp w
      return $ Just pp
