module Warpless.Response
  ( sendResponse,
    hasBody,
    replaceHeader,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.ByteString.Builder.HTTP.Chunked (chunkedTransferEncoding, chunkedTransferTerminator)
import Data.ByteString.Char8 qualified as C8
import Data.CaseInsensitive qualified as CI
import Data.Function (on)
import Data.List qualified as List
import Data.Streaming.ByteString.Builder (newByteStringBuilderRecv, reuseBufferStrategy)
import Data.Tuple (fst)
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as H
import Network.Wai
  ( FilePart (filePartByteCount, filePartOffset),
    Request (httpVersion, requestMethod),
    responseHeaders,
    responseStatus,
  )
import Network.Wai.Internal (Response (..))
import UnliftIO qualified
import Warpless.Byte qualified as Byte
import Warpless.ByteString qualified as ByteString
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.CommonResponseHeaders (CommonResponseHeaders)
import Warpless.CommonResponseHeaders qualified as CommonResponseHeaders
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.Date qualified as D
import Warpless.File (RspFileInfo (..), addContentHeadersForFilePart, conditionalRequest)
import Warpless.FileInfo (getFileInfo)
import Warpless.IO (toBufIOWith)
import Warpless.Prelude
import Warpless.ResponseHeader (composeHeader)
import Warpless.Types (HeaderValue, WeirdClient)
import Warpless.WriteBuffer (toBuilderBuffer)

-- | Sending a HTTP response to 'Connection' according to 'Response'.
--
--   Applications/middlewares MUST provide a proper 'H.ResponseHeaders'.
--   so that inconsistency does not happen.
--   No header is deleted by this function.
--
--   Especially, Applications/middlewares MUST provide a proper
--   Content-Type. They MUST NOT provide
--   Content-Length, Content-Range, and Transfer-Encoding
--   because they are inserted, when necessary,
--   regardless they already exist.
--   This function does not insert Content-Encoding. It's middleware's
--   responsibility.
--
--   The Date and Server header is added if not exist
--   in HTTP response header.
--
--   There are three basic APIs to create 'Response':
--
--   ['responseBuilder' :: 'H.Status' -> 'H.ResponseHeaders' -> 'Builder' -> 'Response']
--     HTTP response body is created from 'Builder'.
--     Transfer-Encoding: chunked is used in HTTP/1.1.
--
--   ['responseStream' :: 'H.Status' -> 'H.ResponseHeaders' -> 'StreamingBody' -> 'Response']
--     HTTP response body is created from 'Builder'.
--     Transfer-Encoding: chunked is used in HTTP/1.1.
--
--   ['responseRaw' :: ('IO' 'ByteString' -> ('ByteString' -> 'IO' ()) -> 'IO' ()) -> 'Response' -> 'Response']
--     No header is added and no Transfer-Encoding: is applied.
--
--   ['responseFile' :: 'H.Status' -> 'H.ResponseHeaders' -> 'FilePath' -> 'Maybe' 'FilePart' -> 'Response']
--     HTTP response body is sent (by sendfile(), if possible) for GET method.
--     HTTP response body is not sent by HEAD method.
--     Content-Length and Content-Range are automatically
--     added into the HTTP response header if necessary.
--     If Content-Length and Content-Range exist in the HTTP response header,
--     they would cause inconsistency.
--     \"Accept-Ranges: bytes\" is also inserted.
--
--     Applications are categorized into simple and sophisticated.
--     Sophisticated applications should specify 'Just' to
--     'Maybe' 'FilePart'. They should treat the conditional request
--     by themselves. A proper 'Status' (200 or 206) must be provided.
--
--     Simple applications should specify 'Nothing' to
--     'Maybe' 'FilePart'. The size of the specified file is obtained
--     by disk access.
--     If-Modified-Since, If-Unmodified-Since, If-Range and Range
--     are processed. Since a proper status is chosen, 'Status' is
--     ignored. Last-Modified is inserted.
sendResponse ::
  Connection ->
  IO D.GMTDate ->
  Request ->
  CommonRequestHeaders ->
  -- | source from client, for raw response
  IO ByteString ->
  -- | HTTP response including status code and response header.
  Response ->
  -- | Returing True if the connection is persistent.
  IO Bool
sendResponse conn getDate request commonRequestHeaders source response = do
  -- If the client disconnects while we're sending it a response, catch that here and return false (meaning this
  -- connection should not be reused).
  (`catch` \(_ :: WeirdClient) -> pure False) do
    headers <- addDate getDate commonResponseHeaders headers0
    let doSendRspNoBody = sendRspNoBody conn ver status headers
    if hasBody status
      then do
        -- The response to HEAD does not have body.
        -- But to handle the conditional requests defined RFC 7232 and
        -- to generate appropriate content-length, content-range,
        -- and status, the response to HEAD is processed here.
        --
        -- See definition of rsp below for proper body stripping.
        case response of
          ResponseFile _ _ path maybePart -> do
            sendRspFile conn ver status headers commonResponseHeaders method path maybePart commonRequestHeaders
            pure isPersist
          ResponseBuilder _ _ body -> do
            if isHead then doSendRspNoBody else sendRspBuilder conn ver status headers body needsChunked
            pure isKeepAlive
          ResponseStream _ _ body -> do
            if isHead then doSendRspNoBody else sendRspStream conn ver status headers body needsChunked
            pure isKeepAlive
          ResponseRaw raw _ -> do
            sendRspRaw conn raw source
            pure False
      else do
        doSendRspNoBody
        pure isPersist
  where
    hasLength = isJust (CommonResponseHeaders.getContentLength commonResponseHeaders)
    headers0 = sanitizeHeaders (responseHeaders response)
    isChunked = not isHead && ver == H.http11
    isHead = method == H.methodHead
    isKeepAlive = isPersist && (isChunked || hasLength)
    isPersist = checkPersist request commonRequestHeaders
    method = requestMethod request
    needsChunked = isChunked && not hasLength
    commonResponseHeaders = CommonResponseHeaders.make headers0
    status = responseStatus response
    ver = httpVersion request

----------------------------------------------------------------

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = List.map (sanitize <$>)
  where
    sanitize v
      | ByteString.containsNewlines v = sanitizeHeaderValue v -- slow path
      | otherwise = v -- fast path

sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v = case C8.lines $ ByteString.filter (/= Byte.cr) v of
  [] -> ""
  x : xs -> C8.intercalate "\r\n" (x : mapMaybe addSpaceIfMissing xs)
  where
    addSpaceIfMissing line = case C8.uncons line of
      Nothing -> Nothing
      Just (first, _)
        | first == ' ' || first == '\t' -> Just line
        | otherwise -> Just $ " " <> line
{-# INLINE sanitizeHeaderValue #-}

----------------------------------------------------------------

sendRspNoBody :: Connection -> H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ()
sendRspNoBody conn ver status headers =
  -- Not adding Content-Length.
  -- User agents treats it as Content-Length: 0.
  composeHeader ver status headers >>= Connection.send conn

sendRspBuilder :: Connection -> H.HttpVersion -> H.Status -> H.ResponseHeaders -> Builder -> Bool -> IO ()
sendRspBuilder conn ver status headers body needsChunked = do
  header <- composeHeaderBuilder ver status headers needsChunked
  let hdrBdy
        | needsChunked =
            header
              <> chunkedTransferEncoding body
              <> chunkedTransferTerminator
        | otherwise = header <> body
  toBufIOWith (Connection.writeBufferRef conn) (Connection.send conn) hdrBdy

sendRspStream :: Connection -> H.HttpVersion -> H.Status -> H.ResponseHeaders -> ((Builder -> IO ()) -> IO () -> IO ()) -> Bool -> IO ()
sendRspStream conn ver status headers streamingBody needsChunked = do
  header <- composeHeaderBuilder ver status headers needsChunked
  writeBuffer <- readIORef (Connection.writeBufferRef conn)
  (recv, finish) <- newByteStringBuilderRecv (reuseBufferStrategy (toBuilderBuffer writeBuffer))
  let send builder = do
        popper <- recv builder
        let loop = do
              bytes <- popper
              when (not (ByteString.null bytes)) do
                Connection.send conn bytes
                loop
        loop
      sendChunk
        | needsChunked = send . chunkedTransferEncoding
        | otherwise = send
  send header
  streamingBody sendChunk (sendChunk flush)
  when needsChunked $ send chunkedTransferTerminator
  mbs <- finish
  for_ mbs (Connection.send conn)

sendRspRaw :: Connection -> (IO ByteString -> (ByteString -> IO ()) -> IO ()) -> IO ByteString -> IO ()
sendRspRaw conn withApp src =
  withApp src (Connection.send conn)

sendRspFile ::
  Connection ->
  H.HttpVersion ->
  H.Status ->
  H.ResponseHeaders ->
  CommonResponseHeaders ->
  H.Method ->
  FilePath ->
  Maybe FilePart ->
  CommonRequestHeaders ->
  IO ()
sendRspFile conn ver status headers rspidxhdr method path maybePart commonRequestHeaders =
  case maybePart of
    -- Simple WAI applications.
    -- Status is ignored
    Nothing ->
      UnliftIO.tryIO (getFileInfo path) >>= \case
        Left _ex -> sendRspFile404 conn ver headers
        Right finfo ->
          case conditionalRequest finfo headers method rspidxhdr commonRequestHeaders of
            WithoutBody status1 -> sendRspNoBody conn ver status1 headers
            WithBody s hs beg len -> sendRspFile2XX conn ver s hs method path beg len
    -- Sophisticated WAI applications.
    -- We respect status. status MUST be a proper value.
    Just part ->
      sendRspFile2XX conn ver status headers1 method path beg len
      where
        beg = filePartOffset part
        len = filePartByteCount part
        headers1 = addContentHeadersForFilePart headers part

sendRspFile2XX ::
  Connection ->
  H.HttpVersion ->
  H.Status ->
  H.ResponseHeaders ->
  H.Method ->
  FilePath ->
  Integer ->
  Integer ->
  IO ()
sendRspFile2XX conn ver status headers method path beg len
  | method == H.methodHead = sendRspNoBody conn ver status headers
  | otherwise = do
      lheader <- composeHeader ver status headers
      Connection.sendfile conn path beg len (pure ()) [lheader]

sendRspFile404 :: Connection -> H.HttpVersion -> H.ResponseHeaders -> IO ()
sendRspFile404 conn ver headers =
  sendRspBuilder conn ver H.notFound404 headers1 body True
  where
    headers1 = replaceHeader H.hContentType "text/plain; charset=utf-8" headers
    body = byteString "File not found"

----------------------------------------------------------------

checkPersist :: Request -> CommonRequestHeaders -> Bool
checkPersist req headers
  | httpVersion req == H.http11 = checkPersist11 conn
  | otherwise = checkPersist10 conn
  where
    conn = CommonRequestHeaders.getConnection headers
    checkPersist11 :: Maybe ByteString -> Bool
    checkPersist11 (Just x) | CI.foldCase x == "close" = False
    checkPersist11 _ = True
    checkPersist10 :: Maybe ByteString -> Bool
    checkPersist10 (Just x) | CI.foldCase x == "keep-alive" = True
    checkPersist10 _ = False

----------------------------------------------------------------

hasBody :: H.Status -> Bool
hasBody (H.statusCode -> code) =
  code /= 204 && code /= 304 && code >= 200

----------------------------------------------------------------

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs =
  (H.hTransferEncoding, "chunked") : hdrs

addDate :: IO D.GMTDate -> CommonResponseHeaders -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate getDate commonHeaders headers =
  case CommonResponseHeaders.getDate commonHeaders of
    Nothing -> do
      gmtdate <- getDate
      pure ((H.hDate, gmtdate) : headers)
    Just _ -> pure headers

----------------------------------------------------------------

-- |
--
-- >>> replaceHeader "Content-Type" "new" [("content-type","old")]
-- [("Content-Type","new")]
replaceHeader :: H.HeaderName -> HeaderValue -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (k, v) : List.deleteBy ((==) `on` fst) (k, v) hdrs

----------------------------------------------------------------

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
  byteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
  byteString <$> composeHeader ver s hs
