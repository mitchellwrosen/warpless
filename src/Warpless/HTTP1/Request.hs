module Warpless.HTTP1.Request
  ( receiveRequest,
  )
where

import Control.Concurrent qualified as Concurrent (yield)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as ByteString.Builder
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.Vault.Lazy qualified as Vault
import GHC.Real (fromIntegral)
import Network.HTTP.Types qualified as Http
import Network.Socket (SockAddr)
import Network.Wai (RequestBodyLength (ChunkedBody, KnownLength))
import Network.Wai.Internal (Request (..))
import Warpless.Byte qualified as Byte
import Warpless.ByteString qualified as ByteString
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.HTTP1.ChunkedSource qualified as ChunkedSource
import Warpless.HTTP1.Source (Source)
import Warpless.HTTP1.Source qualified as Source
import Warpless.HTTP1.SourceN qualified as SourceN
import Warpless.Prelude
import Warpless.RequestHeader (parseHeaderLines)
import Warpless.Settings (Settings, settingsNoParsePath)
import Warpless.Types (WeirdClient (..))

receiveRequest :: Settings -> Connection -> SockAddr -> Source -> IO (Request, CommonRequestHeaders, IO ByteString)
receiveRequest settings conn remoteHost source = do
  headerLines <- readHeaderLines source

  (method, unparsedPath, rawQueryString, httpVersion, requestHeaders) <- parseHeaderLines headerLines

  let path = Http.extractPath unparsedPath
      commonHeaders = CommonRequestHeaders.make requestHeaders

  (getBodyChunk, bodyLength) <-
    if CommonRequestHeaders.hasChunkedTransferEncoding commonHeaders
      then do
        chunkedSource <- ChunkedSource.make source
        pure (ChunkedSource.read chunkedSource, ChunkedBody)
      else do
        let len = maybe 0 (fromIntegral @Int64 @Int . ByteString.readInt64) (CommonRequestHeaders.getContentLength commonHeaders)
        sourceN <- SourceN.new source len
        pure (SourceN.read sourceN, KnownLength (fromIntegral @Int @Word64 len))

  -- Eagerly reply to "Expect: 100-continue" header, if it exists.
  when (CommonRequestHeaders.hasExpect100Continue commonHeaders) do
    Connection.send conn if httpVersion == Http.http11 then "HTTP/1.1 100 Continue\r\n\r\n" else "HTTP/1.0 100 Continue\r\n\r\n"
    Concurrent.yield

  let request =
        Request
          { requestMethod = method,
            httpVersion,
            pathInfo = Http.decodePathSegments path,
            rawPathInfo = if settingsNoParsePath settings then unparsedPath else path,
            rawQueryString,
            queryString = Http.parseQuery rawQueryString,
            requestHeaders,
            isSecure = False,
            remoteHost,
            requestBody = getBodyChunk,
            vault = Vault.empty,
            requestBodyLength = bodyLength,
            requestHeaderHost = CommonRequestHeaders.getHost commonHeaders,
            requestHeaderRange = CommonRequestHeaders.getRange commonHeaders,
            requestHeaderReferer = CommonRequestHeaders.getReferer commonHeaders,
            requestHeaderUserAgent = CommonRequestHeaders.getUserAgent commonHeaders
          }

  pure (request, commonHeaders, getBodyChunk)

----------------------------------------------------------------

readHeaderLines :: Source -> IO [ByteString]
readHeaderLines source = do
  bytes <- Source.read source
  when (ByteString.null bytes) (throwIO WeirdClient)
  push source (THStatus 0 0 id id) bytes

----------------------------------------------------------------

data THStatus
  = THStatus
      {-# UNPACK #-} !Int -- running total byte count (excluding current header chunk)
      {-# UNPACK #-} !Int -- current header chunk byte count
      !([ByteString] -> [ByteString]) -- previously parsed lines
      !(ByteString.Builder.Builder -> ByteString.Builder.Builder) -- bytestrings to be prepended

----------------------------------------------------------------

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push src (THStatus totalLen chunkLen reqLines prepend) bs =
  case ByteString.elemIndex Byte.lf bs of
    -- No newline found
    Nothing -> withNewChunk noNewlineFound
    -- Newline found at index 'ix'
    Just ix -> newlineFound ix
  where
    bsLen = ByteString.length bs
    currentTotal = totalLen + chunkLen
    {-# INLINE withNewChunk #-}
    withNewChunk :: (ByteString -> IO a) -> IO a
    withNewChunk f = do
      newChunk <- Source.readIgnoringLeftovers src
      when (ByteString.null newChunk) $ throwIO WeirdClient
      f newChunk
    {-# INLINE noNewlineFound #-}
    noNewlineFound newChunk
      -- The chunk split the CRLF in half
      | ByteString.Unsafe.unsafeLast bs == Byte.cr && ByteString.head newChunk == Byte.lf =
          let bs' = ByteString.Unsafe.unsafeDrop 1 newChunk
           in if bsLen == 1 && chunkLen == 0
                then -- first part is only CRLF, we're done
                do
                  when (not $ ByteString.null bs') $ Source.setLeftovers src bs'
                  pure $ reqLines []
                else do
                  rest <-
                    if ByteString.length newChunk == 1
                      then -- new chunk is only LF, we need more to check for multiline
                        withNewChunk pure
                      else pure bs'
                  let status = addLine (bsLen + 1) (ByteString.Unsafe.unsafeTake (bsLen - 1) bs)
                  push src status rest
      -- chunk and keep going
      | otherwise = do
          let newChunkTotal = chunkLen + bsLen
              newPrepend = prepend . (ByteString.Builder.byteString bs <>)
              status = THStatus totalLen newChunkTotal reqLines newPrepend
          push src status newChunk
    {-# INLINE newlineFound #-}
    newlineFound ix
      -- Is end of headers
      | startsWithLF && chunkLen == 0 = do
          let rest = ByteString.Unsafe.unsafeDrop end bs
          when (not $ ByteString.null rest) $ Source.setLeftovers src rest
          pure $ reqLines []
      | otherwise = do
          -- LF is on last byte
          rest <-
            if end == bsLen
              then -- we need more chunks to check for whitespace
                withNewChunk pure
              else pure $ ByteString.Unsafe.unsafeDrop end bs
          let p = ix - 1
              chunk =
                if ix > 0 && ByteString.Unsafe.unsafeIndex bs p == Byte.cr then p else ix
              status = addLine end (ByteString.Unsafe.unsafeTake chunk bs)
          push src status rest
      where
        end = ix + 1
        startsWithLF =
          case ix of
            0 -> True
            1 -> ByteString.Unsafe.unsafeHead bs == Byte.cr
            _ -> False
    -- addLine: take the current chunk and, if there's nothing to prepend,
    -- add straight to 'reqLines', otherwise first prepend then add.
    addLine len chunk =
      let newTotal = currentTotal + len
          toBS = ByteString.Lazy.toStrict . ByteString.Builder.toLazyByteString
          newLine =
            if chunkLen == 0 then chunk else toBS $ prepend $ ByteString.Builder.byteString chunk
       in THStatus newTotal 0 (reqLines . (newLine :)) id
