module Warpless.Request
  ( recvRequest,
    headerLines,
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
  -- how many bytes remain to be consumed, if known
  -- 'IndexedHeader' of HTTP request for internal use,
  -- Body producing action used for flushing the request body
  IO
    ( Request,
      Maybe (IO Int),
      IndexedHeader,
      IO ByteString
    )
recvRequest firstRequest settings conn addr src = do
  hdrlines <- headerLines firstRequest src
  (method, unparsedPath, path, query, httpversion, hdr) <- parseHeaderLines hdrlines
  let idxhdr = indexRequestHeader hdr
      expect = idxhdr ! fromEnum ReqExpect
      cl = idxhdr ! fromEnum ReqContentLength
      te = idxhdr ! fromEnum ReqTransferEncoding
      handle100Continue = handleExpect conn httpversion expect
      rawPath = if settingsNoParsePath settings then unparsedPath else path
  (rbody, remainingRef, bodyLength) <-
    if isChunked te
      then do
        csrc <- mkCSource src
        pure (readCSource csrc, Nothing, ChunkedBody)
      else do
        let len = toLength cl
        sourceN <- SourceN.new src len
        pure (SourceN.read sourceN, Just (SourceN.remaining sourceN), KnownLength (fromIntegral @Int @Word64 len))
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
  pure (req, remainingRef, idxhdr, rbody)

----------------------------------------------------------------

headerLines :: Bool -> Source -> IO [ByteString]
headerLines firstRequest src = do
  bs <- readSource src
  when (ByteString.null bs) do
    -- When we're working on a keep-alive connection and trying to
    -- get the second or later request, we don't want to treat the
    -- lack of data as a real exception. See the http1 function in
    -- the Run module for more details.
    if firstRequest
      then throwIO ConnectionClosedByPeer
      else throwIO NoKeepAliveRequest
  push src (THStatus 0 0 id id) bs

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
push src (THStatus totalLen chunkLen lines prepend) bs' =
  case ByteString.elemIndex 10 bs' of
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    Nothing -> do
      bst <- readSource' src
      when (ByteString.null bst) (throwIO IncompleteHeaders)
      push
        src
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
            src
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
              when (start < bsLen) (leftoverSource src (ByteString.Unsafe.unsafeDrop start bs))
              pure (lines [])
            else do
              -- more headers
              let status = THStatus (totalLen + chunkLen + chunkNLlen) 0 (lines . (line :)) id
              case start < bsLen of
                -- more bytes in this chunk, push again
                True -> push src status (ByteString.Unsafe.unsafeDrop start bs)
                -- no more bytes in this chunk, ask for more
                False -> do
                  bst <- readSource' src
                  when (ByteString.null bs) (throwIO IncompleteHeaders)
                  push src status bst
  where
    -- bs: current header chunk, plus maybe (parts of) next header
    bs = prepend bs'
    bsLen = ByteString.length bs

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos =
  if pos > 0 && 13 == ByteString.index bs p then p else pos -- 13 is CR (\r)
  where
    !p = pos - 1
