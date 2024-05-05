module Warpless.HTTP1.Request
  ( receiveRequest,
  )
where

import Control.Concurrent qualified as Concurrent (yield)
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString (unsafeDrop, unsafeHead, unsafeIndex, unsafeLast, unsafeTake)
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

receiveRequest :: Settings -> Connection -> SockAddr -> Source -> IO (Request, CommonRequestHeaders, IO ByteString)
receiveRequest settings conn remoteHost source = do
  headerLines <- do
    bytes <- Source.read1 source
    push source (THStatus 0 0 id id) bytes

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

data THStatus
  = THStatus
      {-# UNPACK #-} !Int -- running total byte count (excluding current header chunk)
      {-# UNPACK #-} !Int -- current header chunk byte count
      !([ByteString] -> [ByteString]) -- previously parsed lines
      !(ByteString -> ByteString) -- bytestrings to be prepended

----------------------------------------------------------------

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push source (THStatus totalLen chunkLen reqLines prepend) bytes0 =
  case ByteString.elemIndex Byte.lf bytes0 of
    -- No newline found
    Nothing -> do
      bytes1 <- Source.readIgnoringLeftovers1 source
      -- The chunk split the CRLF in half
      case ByteString.unsafeLast bytes0 == Byte.cr && ByteString.head bytes1 == Byte.lf of
        True -> do
          let bytes2 = ByteString.unsafeDrop 1 bytes1
          case len0 == 1 && chunkLen == 0 of
            -- first part is only CRLF, we're done
            True -> done bytes2
            False -> do
              let status = addLine (len0 + 1) (ByteString.unsafeTake (len0 - 1) bytes0)
              -- if new chunk is only LF, we need more to check for multiline
              bytes3 <- if ByteString.length bytes1 == 1 then Source.readIgnoringLeftovers1 source else pure bytes2
              push source status bytes3
        -- chunk and keep going
        False -> do
          let prepend1 = prepend . (bytes0 <>)
              status = THStatus totalLen (chunkLen + len0) reqLines prepend1
          push source status bytes1
    -- Newline found at index 'ix'
    Just ix -> do
      let bytes1 = ByteString.unsafeDrop end bytes0
      case chunkLen == 0 && startsWithLF of
        -- Is end of headers
        True -> done bytes1
        False -> do
          -- LF is on last byte
          let p = ix - 1
              chunk = if ix > 0 && ByteString.unsafeIndex bytes0 p == Byte.cr then p else ix
              status = addLine end (ByteString.unsafeTake chunk bytes0)
          if end == len0
            then Source.readIgnoringLeftovers1 source >>= push source status
            else push source status bytes1
      where
        end = ix + 1
        startsWithLF =
          case ix of
            0 -> True
            1 -> ByteString.unsafeHead bytes0 == Byte.cr
            _ -> False
  where
    len0 :: Int
    len0 =
      ByteString.length bytes0

    done :: ByteString -> IO [ByteString]
    done bytes = do
      Source.maybeSetLeftovers source bytes
      pure (reqLines [])

    -- addLine: take the current chunk and, if there's nothing to prepend,
    -- add straight to 'reqLines', otherwise first prepend then add.
    {-# INLINE addLine #-}
    addLine :: Int -> ByteString -> THStatus
    addLine len chunk =
      let newTotal = totalLen + chunkLen + len
          newLine = if chunkLen == 0 then chunk else prepend chunk
       in THStatus newTotal 0 (reqLines . (newLine :)) id
