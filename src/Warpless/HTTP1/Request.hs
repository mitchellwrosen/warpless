module Warpless.HTTP1.Request
  ( receiveRequest,
  )
where

import Control.Concurrent qualified as Concurrent (yield)
import Data.ByteString qualified as ByteString
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
      !(ByteString -> ByteString) -- bytestrings to be prepended

----------------------------------------------------------------

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push source (THStatus totalLen chunkLen lines prepend) bytes =
  case ByteString.elemIndex Byte.lf bytes of
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    Nothing -> do
      bytes1 <- Source.readIgnoringLeftovers source
      when (ByteString.null bytes1) (throwIO WeirdClient)
      push
        source
        (THStatus totalLen (chunkLen + ByteString.length bytes) lines (ByteString.append bs))
        bytes1
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
              when (start < bsLen) (Source.setLeftovers source (ByteString.Unsafe.unsafeDrop start bs))
              pure (lines [])
            else do
              -- more headers
              let status = THStatus (totalLen + chunkLen + chunkNLlen) 0 (lines . (line :)) id
              case start < bsLen of
                -- more bytes in this chunk, push again
                True -> push source status (ByteString.Unsafe.unsafeDrop start bs)
                -- no more bytes in this chunk, ask for more
                False -> do
                  bst <- Source.readIgnoringLeftovers source
                  when (ByteString.null bs) (throwIO WeirdClient)
                  push source status bst
  where
    -- bs: current header chunk, plus maybe (parts of) next header
    bs = prepend bytes
    bsLen = ByteString.length bs

checkCR :: ByteString -> Int -> Int
checkCR bytes pos
  | pos > 0 && Byte.cr == ByteString.index bytes posMinusOne = posMinusOne
  | otherwise = pos
  where
    !posMinusOne = pos - 1
{-# INLINE checkCR #-}
