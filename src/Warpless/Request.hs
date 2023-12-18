module Warpless.Request
  ( recvRequest,
    NoKeepAliveRequest (..),
  )
where

import Control.Concurrent qualified as Concurrent (yield)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Vault.Lazy qualified as Vault
import Data.Word (Word64)
import Network.HTTP.Types qualified as Http
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal (Request (Request))
import Warpless.Byte qualified as Byte
import Warpless.Conduit
import Warpless.Connection (Connection (..), connSend)
import Warpless.Header
import Warpless.ReadInt
import Warpless.RequestHeader (parseHeaderLines)
import Warpless.Settings (Settings, settingsNoParsePath)
import Warpless.Source (Source, leftoverSource, readSource, readSource')
import Warpless.SourceN qualified as SourceN
import Warpless.Types
import Prelude hiding (lines)

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
recvRequest ::
  -- | first request on this connection?
  Bool ->
  Settings ->
  Connection ->
  -- | Peer's address.
  SockAddr ->
  -- | Where HTTP request comes from.
  Source ->
  -- |
  -- 'Request' passed to 'Application',
  -- 'IndexedHeader' of HTTP request for internal use,
  -- Body producing action used for flushing the request body
  IO
    ( Request,
      IndexedHeader,
      IO ByteString
    )
recvRequest isFirstRequest settings conn addr source = do
  hdrlines <- readHeaderLines isFirstRequest source
  (method, unparsedPath, query, httpversion, hdr) <- parseHeaderLines hdrlines
  let path = Http.extractPath unparsedPath
      idxhdr = indexRequestHeader hdr
      expect = idxhdr ! fromEnum ReqExpect
      cl = idxhdr ! fromEnum ReqContentLength
      te = idxhdr ! fromEnum ReqTransferEncoding
      handle100Continue = handleExpect conn httpversion expect
      rawPath = if settingsNoParsePath settings then unparsedPath else path
  (rbody, bodyLength) <-
    if isChunked te
      then do
        csrc <- mkCSource source
        pure (readCSource csrc, ChunkedBody)
      else do
        let len = toLength cl
        sourceN <- SourceN.new source len
        pure (SourceN.read sourceN, KnownLength (fromIntegral @Int @Word64 len))
  -- body producing function which will produce '100-continue', if needed
  rbody' <- timeoutBody rbody handle100Continue
  let req =
        Request
          { requestMethod = method,
            httpVersion = httpversion,
            pathInfo = Http.decodePathSegments path,
            rawPathInfo = rawPath,
            rawQueryString = query,
            queryString = Http.parseQuery query,
            requestHeaders = hdr,
            isSecure = False,
            remoteHost = addr,
            requestBody = rbody',
            vault = Vault.empty,
            requestBodyLength = bodyLength,
            requestHeaderHost = idxhdr ! fromEnum ReqHost,
            requestHeaderRange = idxhdr ! fromEnum ReqRange,
            requestHeaderReferer = idxhdr ! fromEnum ReqReferer,
            requestHeaderUserAgent = idxhdr ! fromEnum ReqUserAgent
          }
  pure (req, idxhdr, rbody)

----------------------------------------------------------------

readHeaderLines :: Bool -> Source -> IO [ByteString]
readHeaderLines isFirstRequest source = do
  bytes <- readSource source
  when (ByteString.null bytes) do
    -- When we're working on a keep-alive connection and trying to
    -- get the second or later request, we don't want to treat the
    -- lack of data as a real exception. See the http1 function in
    -- the Run module for more details.
    if isFirstRequest
      then throwIO ConnectionClosedByPeer
      else throwIO NoKeepAliveRequest
  push source (THStatus 0 0 id id) bytes

data NoKeepAliveRequest = NoKeepAliveRequest
  deriving stock (Show)
  deriving anyclass (Exception)

----------------------------------------------------------------

handleExpect :: Connection -> Http.HttpVersion -> Maybe HeaderValue -> IO ()
handleExpect conn ver = \case
  Just "100-continue" -> do
    let continue :: ByteString
        continue
          | ver == Http.http11 = "HTTP/1.1 100 Continue\r\n\r\n"
          | otherwise = "HTTP/1.0 100 Continue\r\n\r\n"
    connSend conn continue
    Concurrent.yield
  _ -> pure ()

----------------------------------------------------------------

toLength :: Maybe HeaderValue -> Int
toLength = \case
  Nothing -> 0
  Just bs -> readInt bs

isChunked :: Maybe HeaderValue -> Bool
isChunked = \case
  Just bs -> CI.foldCase bs == "chunked"
  _ -> False

----------------------------------------------------------------

timeoutBody :: IO ByteString -> IO () -> IO (IO ByteString)
timeoutBody rbody handle100Continue = do
  isFirstRef <- newIORef True
  pure do
    isFirst <- readIORef isFirstRef
    when isFirst do
      -- Only check if we need to produce the 100 Continue status
      -- when asking for the first chunk of the body
      handle100Continue
      writeIORef isFirstRef False
    rbody

----------------------------------------------------------------

data THStatus
  = THStatus
      {-# UNPACK #-} !Int -- running total byte count (excluding current header chunk)
      {-# UNPACK #-} !Int -- current header chunk byte count
      !([ByteString] -> [ByteString]) -- previously parsed lines
      !(ByteString -> ByteString) -- bytestrings to be prepended

----------------------------------------------------------------

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push source (THStatus totalLen chunkLen lines prepend) bs' =
  case ByteString.elemIndex Byte.newline bs' of
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    Nothing -> do
      bst <- readSource' source
      when (ByteString.null bst) (throwIO IncompleteHeaders)
      push
        source
        (THStatus totalLen (chunkLen + ByteString.length bs') lines (ByteString.append bs))
        bst
    Just chunkNL -> do
      let headerNL = chunkNL + ByteString.length (prepend ByteString.empty)
      let chunkNLlen = chunkNL + 1
      -- check if there are two more bytes in the bs
      -- if so, see if the second of those is a horizontal space
      let isMultiline =
            if bsLen > headerNL + 1
              then
                let c = ByteString.index bs (headerNL + 1)
                    b = case headerNL of
                      0 -> False
                      1 -> ByteString.index bs 0 /= 13
                      _ -> True
                 in b && (c == 32 || c == 9)
              else False
      if isMultiline
        then do
          -- Found a newline, but next line continues as a multiline header
          -- If we'd just update the entire current chunk up to newline
          -- we wouldn't count all the dropped newlines in between.
          -- So update 'chunkLen' with current chunk up to newline
          -- and use 'chunkLen' later on to add to 'totalLen'.
          push
            source
            ( THStatus
                totalLen
                (chunkLen + chunkNLlen)
                lines
                (ByteString.append (ByteString.Unsafe.unsafeTake (checkCR bs headerNL) bs))
            )
            (ByteString.drop (headerNL + 1) bs)
        else do
          -- Found a newline at position end.
          let start = headerNL + 1 -- start of next chunk
          let line = ByteString.Unsafe.unsafeTake (checkCR bs headerNL) bs
          if ByteString.null line
            then do
              -- leftover
              when (start < bsLen) (leftoverSource source (ByteString.Unsafe.unsafeDrop start bs))
              pure (lines [])
            else do
              -- more headers
              let status = THStatus (totalLen + chunkLen + chunkNLlen) 0 (lines . (line :)) id
              case start < bsLen of
                -- more bytes in this chunk, push again
                True -> push source status (ByteString.Unsafe.unsafeDrop start bs)
                -- no more bytes in this chunk, ask for more
                False -> do
                  bst <- readSource' source
                  when (ByteString.null bs) (throwIO IncompleteHeaders)
                  push source status bst
  where
    -- bs: current header chunk, plus maybe (parts of) next header
    bs = prepend bs'
    bsLen = ByteString.length bs

checkCR :: ByteString -> Int -> Int
checkCR bytes pos
  | pos > 0 && Byte.cr == ByteString.index bytes p = p
  | otherwise = pos
  where
    !p = pos - 1
{-# INLINE checkCR #-}
