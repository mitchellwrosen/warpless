module Warpless.Response
  ( sendResponse,
    hasBody,
    replaceHeader,
  )
where

import Control.Monad (when)
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.ByteString.Builder.HTTP.Chunked (chunkedTransferEncoding, chunkedTransferTerminator)
import Data.ByteString.Char8 qualified as C8
import Data.CaseInsensitive qualified as CI
import Data.Foldable (for_)
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (deleteBy)
import Data.Maybe (isJust, mapMaybe)
import Data.Streaming.ByteString.Builder (newByteStringBuilderRecv, reuseBufferStrategy)
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as H
import Network.Wai
import Network.Wai.Internal
import UnliftIO qualified
import Warpless.Byte qualified as Byte
import Warpless.ByteString qualified as ByteString
import Warpless.Connection (Connection, connWriteBuffer)
import Warpless.Connection qualified as Connection
import Warpless.Date qualified as D
import Warpless.File (RspFileInfo (..), addContentHeadersForFilePart, conditionalRequest)
import Warpless.FileInfo (getFileInfo)
import Warpless.Header
import Warpless.IO (toBufIOWith)
import Warpless.ResponseHeader (composeHeader)
import Warpless.Types (HeaderValue)
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
--     by disk access or from the file info cache.
--     If-Modified-Since, If-Unmodified-Since, If-Range and Range
--     are processed. Since a proper status is chosen, 'Status' is
--     ignored. Last-Modified is inserted.
sendResponse ::
  Connection ->
  IO D.GMTDate ->
  -- | HTTP request.
  Request ->
  -- | Indexed header of HTTP request.
  IndexedHeader ->
  -- | source from client, for raw response
  IO ByteString ->
  -- | HTTP response including status code and response header.
  Response ->
  -- | Returing True if the connection is persistent.
  IO Bool
sendResponse conn getDate request reqidxhdr source response = do
  headers <- addDate getDate rspidxhdr headers0
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
          sendRspFile conn ver status headers rspidxhdr method path maybePart reqidxhdr
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
    ver = httpVersion request
    status = responseStatus response
    headers0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader headers0
    isPersist = checkPersist request reqidxhdr
    isChunked = not isHead && ver == H.http11
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr isPersist isChunked
    method = requestMethod request
    isHead = method == H.methodHead

----------------------------------------------------------------

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (sanitize <$>)
  where
    sanitize v
      | ByteString.containsNewlines v = sanitizeHeaderValue v -- slow path
      | otherwise = v -- fast path

sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v = case C8.lines $ S.filter (/= Byte.cr) v of
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
      writeBufferRef = connWriteBuffer conn
  toBufIOWith writeBufferRef (Connection.send conn) hdrBdy

sendRspStream :: Connection -> H.HttpVersion -> H.Status -> H.ResponseHeaders -> ((Builder -> IO ()) -> IO () -> IO ()) -> Bool -> IO ()
sendRspStream conn ver status headers streamingBody needsChunked = do
  header <- composeHeaderBuilder ver status headers needsChunked
  writeBuffer <- readIORef (connWriteBuffer conn)
  (recv, finish) <- newByteStringBuilderRecv (reuseBufferStrategy (toBuilderBuffer writeBuffer))
  let send builder = do
        popper <- recv builder
        let loop = do
              bytes <- popper
              when (not (S.null bytes)) do
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
  IndexedHeader ->
  H.Method ->
  FilePath ->
  Maybe FilePart ->
  IndexedHeader ->
  IO ()
sendRspFile conn ver status headers rspidxhdr method path maybePart reqidxhdr =
  case maybePart of
    -- Simple WAI applications.
    -- Status is ignored
    Nothing ->
      UnliftIO.tryIO (getFileInfo path) >>= \case
        Left _ex -> sendRspFile404 conn ver headers
        Right finfo ->
          case conditionalRequest finfo headers method rspidxhdr reqidxhdr of
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

checkPersist :: Request -> IndexedHeader -> Bool
checkPersist req reqidxhdr
  | httpVersion req == H.http11 = checkPersist11 conn
  | otherwise = checkPersist10 conn
  where
    conn = reqidxhdr ! fromEnum ReqConnection
    checkPersist11 :: Maybe ByteString -> Bool
    checkPersist11 (Just x) | CI.foldCase x == "close" = False
    checkPersist11 _ = True
    checkPersist10 :: Maybe ByteString -> Bool
    checkPersist10 (Just x) | CI.foldCase x == "keep-alive" = True
    checkPersist10 _ = False

----------------------------------------------------------------

-- Used for ResponseBuilder and ResponseSource.
-- Don't use this for ResponseFile since this logic does not fit
-- for ResponseFile. For instance, isKeepAlive should be True in some cases
-- even if the response header does not have Content-Length.
--
-- Content-Length is specified by a reverse proxy.
-- Note that CGI does not specify Content-Length.
infoFromResponse :: IndexedHeader -> Bool -> Bool -> (Bool, Bool)
infoFromResponse rspidxhdr isPersist isChunked = (isKeepAlive, needsChunked)
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive = isPersist && (isChunked || hasLength)
    hasLength = isJust $ rspidxhdr ! fromEnum ResContentLength

----------------------------------------------------------------

hasBody :: H.Status -> Bool
hasBody (H.statusCode -> code) =
  code /= 204 && code /= 304 && code >= 200

----------------------------------------------------------------

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs =
  (H.hTransferEncoding, "chunked") : hdrs

addDate :: IO D.GMTDate -> IndexedHeader -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate getDate rspidxhdr hdrs =
  case rspidxhdr ! fromEnum ResDate of
    Nothing -> do
      gmtdate <- getDate
      pure ((H.hDate, gmtdate) : hdrs)
    Just _ -> pure hdrs

----------------------------------------------------------------

-- |
--
-- >>> replaceHeader "Content-Type" "new" [("content-type","old")]
-- [("Content-Type","new")]
replaceHeader :: H.HeaderName -> HeaderValue -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (k, v) : deleteBy ((==) `on` fst) (k, v) hdrs

----------------------------------------------------------------

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
  byteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
  byteString <$> composeHeader ver s hs
