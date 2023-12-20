module Warpless.HTTP1.Request
  ( receiveRequest,
  )
where

import Control.Concurrent qualified as Concurrent (yield)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Vault.Lazy qualified as Vault
import Data.Word (Word64)
import Network.HTTP.Types qualified as Http
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal (Request (Request))
import Warpless.Byte qualified as Byte
import Warpless.ByteString qualified as ByteString
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.Conduit (mkCSource, readCSource)
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.RequestHeader (parseHeaderLines)
import Warpless.Settings (Settings, settingsNoParsePath)
import Warpless.Source (Source, leftoverSource, readSource, readSource')
import Warpless.SourceN qualified as SourceN
import Warpless.Types (HeaderValue, WeirdClient (..))
import Prelude hiding (lines)

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
--
-- Can throw: 'WeirdClient'
receiveRequest ::
  Settings ->
  Connection ->
  -- | Peer's address.
  SockAddr ->
  -- | Where HTTP request comes from.
  Source ->
  IO
    ( Request,
      CommonRequestHeaders,
      IO ByteString
    )
receiveRequest settings conn addr source = do
  hdrlines <- readHeaderLines source
  (method, unparsedPath, query, httpversion, headers) <- parseHeaderLines hdrlines
  let path = Http.extractPath unparsedPath
      commonHeaders = CommonRequestHeaders.make headers
      handle100Continue = handleExpect conn httpversion (CommonRequestHeaders.getExpect commonHeaders)
      rawPath = if settingsNoParsePath settings then unparsedPath else path
  (rbody, bodyLength) <-
    if CommonRequestHeaders.hasChunkedTransferEncoding commonHeaders
      then do
        csrc <- mkCSource source
        pure (readCSource csrc, ChunkedBody)
      else do
        let len = toLength (CommonRequestHeaders.getContentLength commonHeaders)
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
            requestHeaders = headers,
            isSecure = False,
            remoteHost = addr,
            requestBody = rbody',
            vault = Vault.empty,
            requestBodyLength = bodyLength,
            requestHeaderHost = CommonRequestHeaders.getHost commonHeaders,
            requestHeaderRange = CommonRequestHeaders.getRange commonHeaders,
            requestHeaderReferer = CommonRequestHeaders.getReferer commonHeaders,
            requestHeaderUserAgent = CommonRequestHeaders.getUserAgent commonHeaders
          }
  pure (req, commonHeaders, rbody)

----------------------------------------------------------------

-- Can throw: 'WeirdClient'
readHeaderLines :: Source -> IO [ByteString]
readHeaderLines source = do
  bytes <- readSource source
  when (ByteString.null bytes) do
    throwIO WeirdClient
  push source (THStatus 0 0 id id) bytes

----------------------------------------------------------------

handleExpect :: Connection -> Http.HttpVersion -> Maybe HeaderValue -> IO ()
handleExpect conn ver = \case
  Just "100-continue" -> do
    Connection.send conn if ver == Http.http11 then "HTTP/1.1 100 Continue\r\n\r\n" else "HTTP/1.0 100 Continue\r\n\r\n"
    Concurrent.yield
  _ -> pure ()

----------------------------------------------------------------

toLength :: Maybe HeaderValue -> Int
toLength = \case
  Nothing -> 0
  Just bytes -> fromIntegral @Int64 @Int (ByteString.readInt64 bytes)

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
  case ByteString.elemIndex Byte.lf bs' of
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    Nothing -> do
      bst <- readSource' source
      when (ByteString.null bst) (throwIO WeirdClient)
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
                  when (ByteString.null bs) (throwIO WeirdClient)
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
