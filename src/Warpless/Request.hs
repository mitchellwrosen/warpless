{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Warpless.Request
  ( recvRequest,
    headerLines,
    getFileInfoKey,
    NoKeepAliveRequest (..),
  )
where

import Control.Concurrent qualified as Conc (yield)
import Control.Monad (when)
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Unsafe qualified as SU
import Data.CaseInsensitive qualified as CI
import Data.IORef qualified as I
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types qualified as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (Exception, throwIO)
import Warpless.Conduit
import Warpless.Connection (Connection (..))
import Warpless.FileInfoCache
import Warpless.Header
import Warpless.ReadInt
import Warpless.RequestHeader
import Warpless.Settings (Settings, settingsMaxTotalHeaderLength, settingsNoParsePath)
import Warpless.Types
import Prelude hiding (lines)

----------------------------------------------------------------

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
recvRequest ::
  -- | first request on this connection?
  Bool ->
  Settings ->
  Connection ->
  InternalInfo ->
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
      Maybe (I.IORef Int),
      IndexedHeader,
      IO ByteString
    )
recvRequest firstRequest settings conn ii addr src = do
  hdrlines <- headerLines (settingsMaxTotalHeaderLength settings) firstRequest src
  (method, unparsedPath, path, query, httpversion, hdr) <- parseHeaderLines hdrlines
  let idxhdr = indexRequestHeader hdr
      expect = idxhdr ! fromEnum ReqExpect
      cl = idxhdr ! fromEnum ReqContentLength
      te = idxhdr ! fromEnum ReqTransferEncoding
      handle100Continue = handleExpect conn httpversion expect
      rawPath = if settingsNoParsePath settings then unparsedPath else path
      vaultValue =
        Vault.insert
          getFileInfoKey
          (getFileInfo ii)
          Vault.empty
  (rbody, remainingRef, bodyLength) <- bodyAndSource src cl te
  -- body producing function which will produce '100-continue', if needed
  rbody' <- timeoutBody rbody handle100Continue
  -- body producing function which will never produce 100-continue
  rbodyFlush <- timeoutBody rbody (return ())
  let req =
        Request
          { requestMethod = method,
            httpVersion = httpversion,
            pathInfo = H.decodePathSegments path,
            rawPathInfo = rawPath,
            rawQueryString = query,
            queryString = H.parseQuery query,
            requestHeaders = hdr,
            isSecure = False,
            remoteHost = addr,
            requestBody = rbody',
            vault = vaultValue,
            requestBodyLength = bodyLength,
            requestHeaderHost = idxhdr ! fromEnum ReqHost,
            requestHeaderRange = idxhdr ! fromEnum ReqRange,
            requestHeaderReferer = idxhdr ! fromEnum ReqReferer,
            requestHeaderUserAgent = idxhdr ! fromEnum ReqUserAgent
          }
  return (req, remainingRef, idxhdr, rbodyFlush)

----------------------------------------------------------------

headerLines :: Int -> Bool -> Source -> IO [ByteString]
headerLines maxTotalHeaderLength firstRequest src = do
  bs <- readSource src
  if S.null bs
    then -- When we're working on a keep-alive connection and trying to
    -- get the second or later request, we don't want to treat the
    -- lack of data as a real exception. See the http1 function in
    -- the Run module for more details.
      if firstRequest then throwIO ConnectionClosedByPeer else throwIO NoKeepAliveRequest
    else push maxTotalHeaderLength src (THStatus 0 0 id id) bs

data NoKeepAliveRequest = NoKeepAliveRequest
  deriving stock (Show)
  deriving anyclass (Exception)

----------------------------------------------------------------

handleExpect ::
  Connection ->
  H.HttpVersion ->
  Maybe HeaderValue ->
  IO ()
handleExpect conn ver = \case
  Just "100-continue" -> do
    let continue :: ByteString
        continue
          | ver == H.http11 = "HTTP/1.1 100 Continue\r\n\r\n"
          | otherwise = "HTTP/1.0 100 Continue\r\n\r\n"
    connSendAll conn continue
    Conc.yield
  _ -> return ()

----------------------------------------------------------------

bodyAndSource ::
  Source ->
  -- | content length
  Maybe HeaderValue ->
  -- | transfer-encoding
  Maybe HeaderValue ->
  IO
    ( IO ByteString,
      Maybe (I.IORef Int),
      RequestBodyLength
    )
bodyAndSource src cl te
  | chunked = do
      csrc <- mkCSource src
      return (readCSource csrc, Nothing, ChunkedBody)
  | otherwise = do
      isrc@(ISource _ remaining) <- mkISource src len
      return (readISource isrc, Just remaining, bodyLen)
  where
    len = toLength cl
    bodyLen = KnownLength $ fromIntegral len
    chunked = isChunked te

toLength :: Maybe HeaderValue -> Int
toLength Nothing = 0
toLength (Just bs) = readInt bs

isChunked :: Maybe HeaderValue -> Bool
isChunked (Just bs) = CI.foldCase bs == "chunked"
isChunked _ = False

----------------------------------------------------------------

timeoutBody ::
  IO ByteString ->
  IO () ->
  IO (IO ByteString)
timeoutBody rbody handle100Continue = do
  isFirstRef <- I.newIORef True

  pure do
    isFirst <- I.readIORef isFirstRef

    when isFirst $ do
      -- Only check if we need to produce the 100 Continue status
      -- when asking for the first chunk of the body
      handle100Continue
      I.writeIORef isFirstRef False

    rbody

----------------------------------------------------------------

type BSEndo = ByteString -> ByteString

type BSEndoList = [ByteString] -> [ByteString]

data THStatus
  = THStatus
      !Int -- running total byte count (excluding current header chunk)
      !Int -- current header chunk byte count
      !BSEndoList -- previously parsed lines
      !BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

{- FIXME
close :: Sink ByteString IO a
close = throwIO IncompleteHeaders
-}

push :: Int -> Source -> THStatus -> ByteString -> IO [ByteString]
push maxTotalHeaderLength src (THStatus totalLen chunkLen lines prepend) bs'
  -- Too many bytes
  | currentTotal > maxTotalHeaderLength = throwIO OverLargeHeader
  | otherwise = push' mNL
  where
    currentTotal = totalLen + chunkLen
    -- bs: current header chunk, plus maybe (parts of) next header
    bs = prepend bs'
    bsLen = S.length bs
    -- Maybe newline
    -- Returns: Maybe
    --    ( length of this chunk up to newline
    --    , position of newline in relation to entire current header
    --    , is this part of a multiline header
    --    )
    mNL = do
      -- 10 is the code point for newline (\n)
      chunkNL <- S.elemIndex 10 bs'
      let headerNL = chunkNL + S.length (prepend "")
          chunkNLlen = chunkNL + 1
      -- check if there are two more bytes in the bs
      -- if so, see if the second of those is a horizontal space
      if bsLen > headerNL + 1
        then
          let c = S.index bs (headerNL + 1)
              b = case headerNL of
                0 -> True
                1 -> S.index bs 0 == 13
                _ -> False
              isMultiline = not b && (c == 32 || c == 9)
           in Just (chunkNLlen, headerNL, isMultiline)
        else Just (chunkNLlen, headerNL, False)

    {-# INLINE push' #-}
    push' :: Maybe (Int, Int, Bool) -> IO [ByteString]
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    push' Nothing = do
      bst <- readSource' src
      when (S.null bst) $ throwIO IncompleteHeaders
      push maxTotalHeaderLength src status bst
      where
        prepend' = S.append bs
        thisChunkLen = S.length bs'
        newChunkLen = chunkLen + thisChunkLen
        status = THStatus totalLen newChunkLen lines prepend'
    -- Found a newline, but next line continues as a multiline header
    push' (Just (chunkNLlen, end, True)) =
      push maxTotalHeaderLength src status rest
      where
        rest = S.drop (end + 1) bs
        prepend' = S.append (SU.unsafeTake (checkCR bs end) bs)
        -- If we'd just update the entire current chunk up to newline
        -- we wouldn't count all the dropped newlines in between.
        -- So update 'chunkLen' with current chunk up to newline
        -- and use 'chunkLen' later on to add to 'totalLen'.
        newChunkLen = chunkLen + chunkNLlen
        status = THStatus totalLen newChunkLen lines prepend'
    -- Found a newline at position end.
    push' (Just (chunkNLlen, end, False))
      -- leftover
      | S.null line = do
          when (start < bsLen) $ leftoverSource src (SU.unsafeDrop start bs)
          return (lines [])
      -- more headers
      | otherwise =
          let lines' = lines . (line :)
              newTotalLength = totalLen + chunkLen + chunkNLlen
              status = THStatus newTotalLength 0 lines' id
           in if start < bsLen
                then -- more bytes in this chunk, push again

                  let bs'' = SU.unsafeDrop start bs
                   in push maxTotalHeaderLength src status bs''
                else do
                  -- no more bytes in this chunk, ask for more
                  bst <- readSource' src
                  when (S.null bs) $ throwIO IncompleteHeaders
                  push maxTotalHeaderLength src status bst
      where
        start = end + 1 -- start of next chunk
        line = SU.unsafeTake (checkCR bs end) bs

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos = if pos > 0 && 13 == S.index bs p then p else pos -- 13 is CR (\r)
  where
    !p = pos - 1

getFileInfoKey :: Vault.Key (FilePath -> IO FileInfo)
getFileInfoKey = unsafePerformIO Vault.newKey
{-# NOINLINE getFileInfoKey #-}
