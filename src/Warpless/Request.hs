module Warpless.Request
  ( receiveRequestHeaders,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString (unsafeDrop, unsafeHead, unsafeIndex, unsafeLast, unsafeTake)
import Network.HTTP.Types qualified as Http
import Warpless.Byte qualified as Byte
import Warpless.Prelude
import Warpless.RequestHeader (parseHeaderLines)
import Warpless.Source (Source)
import Warpless.Source qualified as Source

receiveRequestHeaders :: Source -> IO (Http.Method, ByteString, ByteString, Http.HttpVersion, Http.RequestHeaders)
receiveRequestHeaders source = do
  headerLines <- do
    bytes <- Source.read1 source
    push source (THStatus 0 id id) bytes
  parseHeaderLines headerLines

data THStatus
  = THStatus
  { chunkLen :: {-# UNPACK #-} !Int, -- current header chunk byte count
    reqLines :: !([ByteString] -> [ByteString]), -- previously parsed lines
    prepend :: !(ByteString -> ByteString) -- bytestrings to be prepended
  }

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push source status bytes0 =
  case ByteString.elemIndex Byte.lf bytes0 of
    -- No newline found
    Nothing -> do
      bytes1 <- Source.readIgnoringLeftovers1 source
      -- The chunk split the CRLF in half
      case ByteString.unsafeLast bytes0 == Byte.cr && ByteString.head bytes1 == Byte.lf of
        True -> do
          let bytes2 = ByteString.unsafeDrop 1 bytes1
          case len0 == 1 && status.chunkLen == 0 of
            -- first part is only CRLF, we're done
            True -> done bytes2
            False -> do
              -- if new chunk is only LF, we need more to check for multiline
              bytes3 <- if ByteString.length bytes1 == 1 then Source.readIgnoringLeftovers1 source else pure bytes2
              push
                source
                (addLine (ByteString.unsafeTake (len0 - 1) bytes0))
                bytes3
        -- chunk and keep going
        False ->
          push
            source
            status
              { chunkLen = status.chunkLen + len0,
                prepend = status.prepend . (bytes0 <>)
              }
            bytes1
    -- Newline found at index 'ix'
    Just ix -> do
      let bytes1 = ByteString.unsafeDrop end bytes0
      case status.chunkLen == 0 && startsWithLF of
        -- Is end of headers
        True -> done bytes1
        False -> do
          -- LF is on last byte
          let p = ix - 1
              chunk = if ix > 0 && ByteString.unsafeIndex bytes0 p == Byte.cr then p else ix
              status1 = addLine (ByteString.unsafeTake chunk bytes0)
          if end == len0
            then Source.readIgnoringLeftovers1 source >>= push source status1
            else push source status1 bytes1
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
      pure (status.reqLines [])

    -- addLine: take the current chunk and, if there's nothing to prepend,
    -- add straight to 'reqLines', otherwise first prepend then add.
    addLine :: ByteString -> THStatus
    addLine chunk =
      let newLine = if status.chunkLen == 0 then chunk else status.prepend chunk
       in THStatus 0 (status.reqLines . (newLine :)) id
    {-# INLINE addLine #-}
