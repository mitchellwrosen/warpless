module Warpless.HTTP2.Response
  ( fromResponse,
  )
where

import Data.ByteString.Builder qualified as BB
import Network.HTTP.Types qualified as H
import Network.HTTP2.Server qualified as H2
import Network.Wai hiding (responseBuilder, responseFile, responseStream)
import Network.Wai.Internal (Response (..))
import UnliftIO qualified
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.CommonResponseHeaders qualified as CommonResponseHeaders
import Warpless.Date (GMTDate)
import Warpless.File (RspFileInfo (WithBody, WithoutBody), conditionalRequest)
import Warpless.FileInfo (getFileInfo)
import Warpless.HTTP1.Response qualified as HTTP1.Response
import Warpless.HTTP2.Request (getHTTP2Data)
import Warpless.HTTP2.Types (HTTP2Data (http2dataTrailers))
import Warpless.Prelude

----------------------------------------------------------------

fromResponse :: IO GMTDate -> Request -> Response -> IO (H2.Response, Bool)
fromResponse getDate req rsp = do
  date <- getDate
  let addDate = ((H.hDate, date) :)
  rspst@(h2rsp, hasBody) <-
    case rsp of
      ResponseFile st rsphdr path mpart -> responseFile st (addDate rsphdr) method path mpart reqhdr
      ResponseBuilder st rsphdr builder -> pure (responseBuilder st (addDate rsphdr) method builder)
      ResponseStream st rsphdr strmbdy -> pure (responseStream st (addDate rsphdr) method strmbdy)
      _ -> error "ResponseRaw is not supported in HTTP/2"
  mh2data <- getHTTP2Data req
  case mh2data of
    Nothing -> pure rspst
    Just h2data -> do
      let !trailers = http2dataTrailers h2data
          !h2rsp' = H2.setResponseTrailersMaker h2rsp trailers
      pure (h2rsp', hasBody)
  where
    !method = requestMethod req
    !reqhdr = requestHeaders req

----------------------------------------------------------------

responseFile ::
  H.Status ->
  H.ResponseHeaders ->
  H.Method ->
  FilePath ->
  Maybe FilePart ->
  H.RequestHeaders ->
  IO (H2.Response, Bool)
responseFile st rsphdr _ _ _ _ | noBody st = pure (responseNoBody st rsphdr)
responseFile st rsphdr method path (Just fp) _ =
  pure (responseFile2XX st rsphdr method fileSpec)
  where
    !off' = filePartOffset fp
    !bytes' = filePartByteCount fp
    !fileSpec = H2.FileSpec path (unsafeFrom @Integer @Int64 off') (unsafeFrom @Integer @Int64 bytes')
responseFile _ rsphdr method path Nothing reqhdr = do
  efinfo <- UnliftIO.tryIO $ getFileInfo path
  case efinfo of
    Left (_ex :: UnliftIO.IOException) -> pure (response404 rsphdr)
    Right finfo -> do
      let commonRequestHeaders = CommonRequestHeaders.make reqhdr
          commonResponseHeaders = CommonResponseHeaders.make rsphdr
      case conditionalRequest finfo rsphdr method commonResponseHeaders commonRequestHeaders of
        WithoutBody s -> pure (responseNoBody s rsphdr)
        WithBody s rsphdr' off bytes -> do
          let !off' = off
              !bytes' = bytes
              !fileSpec = H2.FileSpec path (unsafeFrom @Integer @Int64 off') (unsafeFrom @Integer @Int64 bytes')
          pure (responseFile2XX s rsphdr' method fileSpec)

----------------------------------------------------------------

responseFile2XX :: H.Status -> H.ResponseHeaders -> H.Method -> H2.FileSpec -> (H2.Response, Bool)
responseFile2XX st rsphdr method fileSpec
  | method == H.methodHead = responseNoBody st rsphdr
  | otherwise = (H2.responseFile st rsphdr fileSpec, True)

----------------------------------------------------------------

responseBuilder :: H.Status -> H.ResponseHeaders -> H.Method -> BB.Builder -> (H2.Response, Bool)
responseBuilder st rsphdr method builder
  | method == H.methodHead || noBody st = responseNoBody st rsphdr
  | otherwise = (H2.responseBuilder st rsphdr builder, True)

----------------------------------------------------------------

responseStream :: H.Status -> H.ResponseHeaders -> H.Method -> StreamingBody -> (H2.Response, Bool)
responseStream st rsphdr method strmbdy
  | method == H.methodHead || noBody st = responseNoBody st rsphdr
  | otherwise = (H2.responseStreaming st rsphdr strmbdy, True)

----------------------------------------------------------------

responseNoBody :: H.Status -> H.ResponseHeaders -> (H2.Response, Bool)
responseNoBody st rsphdr = (H2.responseNoBody st rsphdr, False)

----------------------------------------------------------------

response404 :: H.ResponseHeaders -> (H2.Response, Bool)
response404 rsphdr = (h2rsp, True)
  where
    h2rsp = H2.responseBuilder st rsphdr' body
    st = H.notFound404
    !rsphdr' = HTTP1.Response.replaceHeader H.hContentType "text/plain; charset=utf-8" rsphdr
    !body = BB.byteString "File not found"

----------------------------------------------------------------

noBody :: H.Status -> Bool
noBody = not . HTTP1.Response.hasBody
