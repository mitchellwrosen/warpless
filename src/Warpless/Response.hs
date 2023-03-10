module Warpless.Response
  ( sendResponse,
    sanitizeHeaderValue, -- for testing
    warpVersion,
    hasBody,
    replaceHeader,
    addServer, -- testing
    addAltSvc,
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
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (deleteBy)
import Data.Maybe (isJust, mapMaybe)
import Data.Streaming.ByteString.Builder (newByteStringBuilderRecv, reuseBufferStrategy)
import Data.Version (showVersion)
import Data.Word8 (_cr, _lf)
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as H
import Network.Wai
import Network.Wai.Internal
import Paths_warpless qualified
import UnliftIO qualified
import Warpless.Connection (Connection (..))
import Warpless.Date qualified as D
import Warpless.File
import Warpless.Header
import Warpless.IO (toBufIOWith)
import Warpless.ResponseHeader
import Warpless.Settings
import Warpless.Types
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
--     by disk access or from the file infor cache.
--     If-Modified-Since, If-Unmodified-Since, If-Range and Range
--     are processed. Since a proper status is chosen, 'Status' is
--     ignored. Last-Modified is inserted.
sendResponse ::
  Settings ->
  Connection ->
  InternalInfo ->
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
sendResponse settings conn ii req reqidxhdr src response = do
  hs <- addAltSvc settings <$> addServerAndDate hs0
  if hasBody s
    then do
      -- The response to HEAD does not have body.
      -- But to handle the conditional requests defined RFC 7232 and
      -- to generate appropriate content-length, content-range,
      -- and status, the response to HEAD is processed here.
      --
      -- See definition of rsp below for proper body stripping.
      (ms, mlen) <- sendRsp conn ii ver s hs rspidxhdr maxRspBufSize rsp
      case ms of
        Nothing -> pure ()
        Just realStatus -> logger req realStatus mlen
      pure ret
    else do
      _ <- sendRsp conn ii ver s hs rspidxhdr maxRspBufSize RspNoBody
      logger req s Nothing
      pure isPersist
  where
    defServer = settingsServerName settings
    logger = settingsLogger settings
    maxRspBufSize = settingsMaxBuilderResponseBufferSize settings
    ver = httpVersion req
    s = responseStatus response
    hs0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    getdate = getDate ii
    addServerAndDate = addDate getdate rspidxhdr . addServer defServer rspidxhdr
    (isPersist, isChunked0) = infoFromRequest req reqidxhdr
    isChunked = not isHead && isChunked0
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr (isPersist, isChunked)
    isHead = requestMethod req == H.methodHead
    rsp = case response of
      ResponseFile _ _ path mPart -> RspFile path mPart reqidxhdr isHead
      ResponseBuilder _ _ b
        | isHead -> RspNoBody
        | otherwise -> RspBuilder b needsChunked
      ResponseStream _ _ fb
        | isHead -> RspNoBody
        | otherwise -> RspStream fb needsChunked
      ResponseRaw raw _ -> RspRaw raw src
    -- Make sure we don't hang on to 'response' (avoid space leak)
    !ret = case response of
      ResponseFile {} -> isPersist
      ResponseBuilder {} -> isKeepAlive
      ResponseStream {} -> isKeepAlive
      ResponseRaw {} -> False

----------------------------------------------------------------

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (sanitize <$>)
  where
    sanitize v
      | containsNewlines v = sanitizeHeaderValue v -- slow path
      | otherwise = v -- fast path

{-# INLINE containsNewlines #-}
containsNewlines :: ByteString -> Bool
containsNewlines = S.any (\w -> w == _cr || w == _lf)

{-# INLINE sanitizeHeaderValue #-}
sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v = case C8.lines $ S.filter (/= _cr) v of
  [] -> ""
  x : xs -> C8.intercalate "\r\n" (x : mapMaybe addSpaceIfMissing xs)
  where
    addSpaceIfMissing line = case C8.uncons line of
      Nothing -> Nothing
      Just (first, _)
        | first == ' ' || first == '\t' -> Just line
        | otherwise -> Just $ " " <> line

----------------------------------------------------------------

data Rsp
  = RspNoBody
  | RspFile !FilePath !(Maybe FilePart) !IndexedHeader !Bool
  | RspBuilder !Builder !Bool
  | RspStream !StreamingBody !Bool
  | RspRaw !(IO ByteString -> (ByteString -> IO ()) -> IO ()) !(IO ByteString)

----------------------------------------------------------------

sendRsp ::
  Connection ->
  InternalInfo ->
  H.HttpVersion ->
  H.Status ->
  H.ResponseHeaders ->
  IndexedHeader -> -- Response
  Int -> -- maxBuilderResponseBufferSize
  Rsp ->
  IO (Maybe H.Status, Maybe Integer)
----------------------------------------------------------------

sendRsp conn _ ver s hs _ _ RspNoBody = do
  -- Not adding Content-Length.
  -- User agents treats it as Content-Length: 0.
  composeHeader ver s hs >>= connSend conn
  return (Just s, Nothing)

----------------------------------------------------------------

sendRsp conn _ ver s hs _ maxRspBufSize (RspBuilder body needsChunked) = do
  header <- composeHeaderBuilder ver s hs needsChunked
  let hdrBdy
        | needsChunked =
            header <> chunkedTransferEncoding body
              <> chunkedTransferTerminator
        | otherwise = header <> body
      writeBufferRef = connWriteBuffer conn
  toBufIOWith maxRspBufSize writeBufferRef (connSend conn) hdrBdy
  return (Just s, Nothing) -- fixme: can we tell the actual sent bytes?

----------------------------------------------------------------

sendRsp conn _ ver s hs _ _ (RspStream streamingBody needsChunked) = do
  header <- composeHeaderBuilder ver s hs needsChunked
  writeBuffer <- readIORef (connWriteBuffer conn)
  (recv, finish) <- newByteStringBuilderRecv (reuseBufferStrategy (toBuilderBuffer writeBuffer))
  let send builder = do
        popper <- recv builder
        let loop = do
              bs <- popper
              when (not (S.null bs)) do
                connSend conn bs
                loop
        loop
      sendChunk
        | needsChunked = send . chunkedTransferEncoding
        | otherwise = send
  send header
  streamingBody sendChunk (sendChunk flush)
  when needsChunked $ send chunkedTransferTerminator
  mbs <- finish
  maybe (return ()) (connSend conn) mbs
  return (Just s, Nothing) -- fixme: can we tell the actual sent bytes?

----------------------------------------------------------------

sendRsp conn _ _ _ _ _ _ (RspRaw withApp src) = do
  withApp src (connSend conn)
  return (Nothing, Nothing)

----------------------------------------------------------------

-- Sophisticated WAI applications.
-- We respect s0. s0 MUST be a proper value.
sendRsp conn ii ver s0 hs0 rspidxhdr maxRspBufSize (RspFile path (Just part) _ isHead) =
  sendRspFile2XX conn ii ver s0 hs rspidxhdr maxRspBufSize path beg len isHead
  where
    beg = filePartOffset part
    len = filePartByteCount part
    hs = addContentHeadersForFilePart hs0 part

----------------------------------------------------------------

-- Simple WAI applications.
-- Status is ignored
sendRsp conn ii ver _ hs0 rspidxhdr maxRspBufSize (RspFile path Nothing reqidxhdr isHead) = do
  efinfo <- UnliftIO.tryIO $ getFileInfo ii path
  case efinfo of
    Left (_ex :: UnliftIO.IOException) ->
      sendRspFile404 conn ii ver hs0 rspidxhdr maxRspBufSize
    Right finfo -> case conditionalRequest finfo hs0 rspidxhdr reqidxhdr of
      WithoutBody s -> sendRsp conn ii ver s hs0 rspidxhdr maxRspBufSize RspNoBody
      WithBody s hs beg len -> sendRspFile2XX conn ii ver s hs rspidxhdr maxRspBufSize path beg len isHead

----------------------------------------------------------------

sendRspFile2XX ::
  Connection ->
  InternalInfo ->
  H.HttpVersion ->
  H.Status ->
  H.ResponseHeaders ->
  IndexedHeader ->
  Int ->
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO (Maybe H.Status, Maybe Integer)
sendRspFile2XX conn ii ver s hs rspidxhdr maxRspBufSize path beg len isHead
  | isHead = sendRsp conn ii ver s hs rspidxhdr maxRspBufSize RspNoBody
  | otherwise = do
      lheader <- composeHeader ver s hs
      (mfd, fresher) <- getFd ii path
      let fid = FileId path mfd
      connSendFile conn fid beg len fresher [lheader]
      return (Just s, Just len)

sendRspFile404 ::
  Connection ->
  InternalInfo ->
  H.HttpVersion ->
  H.ResponseHeaders ->
  IndexedHeader ->
  Int ->
  IO (Maybe H.Status, Maybe Integer)
sendRspFile404 conn ii ver hs0 rspidxhdr maxRspBufSize = sendRsp conn ii ver s hs rspidxhdr maxRspBufSize (RspBuilder body True)
  where
    s = H.notFound404
    hs = replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
    body = byteString "File not found"

----------------------------------------------------------------

infoFromRequest ::
  Request ->
  IndexedHeader ->
  ( Bool, -- isPersist
    Bool -- isChunked
  )
infoFromRequest req reqidxhdr = (checkPersist req reqidxhdr, checkChunk req)

checkPersist :: Request -> IndexedHeader -> Bool
checkPersist req reqidxhdr
  | ver == H.http11 = checkPersist11 conn
  | otherwise = checkPersist10 conn
  where
    ver = httpVersion req
    conn = reqidxhdr ! fromEnum ReqConnection
    checkPersist11 :: Maybe ByteString -> Bool
    checkPersist11 (Just x)
      | CI.foldCase x == "close" = False
    checkPersist11 _ = True
    checkPersist10 :: Maybe ByteString -> Bool
    checkPersist10 (Just x)
      | CI.foldCase x == "keep-alive" = True
    checkPersist10 _ = False

checkChunk :: Request -> Bool
checkChunk req = httpVersion req == H.http11

----------------------------------------------------------------

-- Used for ResponseBuilder and ResponseSource.
-- Don't use this for ResponseFile since this logic does not fit
-- for ResponseFile. For instance, isKeepAlive should be True in some cases
-- even if the response header does not have Content-Length.
--
-- Content-Length is specified by a reverse proxy.
-- Note that CGI does not specify Content-Length.
infoFromResponse :: IndexedHeader -> (Bool, Bool) -> (Bool, Bool)
infoFromResponse rspidxhdr (isPersist, isChunked) = (isKeepAlive, needsChunked)
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive = isPersist && (isChunked || hasLength)
    hasLength = isJust $ rspidxhdr ! fromEnum ResContentLength

----------------------------------------------------------------

hasBody :: H.Status -> Bool
hasBody s =
  sc /= 204
    && sc /= 304
    && sc >= 200
  where
    sc = H.statusCode s

----------------------------------------------------------------

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs = (H.hTransferEncoding, "chunked") : hdrs

addDate :: IO D.GMTDate -> IndexedHeader -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate getdate rspidxhdr hdrs = case rspidxhdr ! fromEnum ResDate of
  Nothing -> do
    gmtdate <- getdate
    return $ (H.hDate, gmtdate) : hdrs
  Just _ -> return hdrs

----------------------------------------------------------------

-- | The version of Warp.
warpVersion :: String
warpVersion = showVersion Paths_warpless.version

{-# INLINE addServer #-}
addServer :: HeaderValue -> IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer "" rspidxhdr hdrs = case rspidxhdr ! fromEnum ResServer of
  Nothing -> hdrs
  _ -> filter ((/= H.hServer) . fst) hdrs
addServer serverName rspidxhdr hdrs = case rspidxhdr ! fromEnum ResServer of
  Nothing -> (H.hServer, serverName) : hdrs
  _ -> hdrs

addAltSvc :: Settings -> H.ResponseHeaders -> H.ResponseHeaders
addAltSvc settings hs = case settingsAltSvc settings of
  Nothing -> hs
  Just v -> ("Alt-Svc", v) : hs

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
