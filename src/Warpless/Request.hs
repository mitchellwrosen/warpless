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
    push source 0 listBuilderEmpty id bytes
  parseHeaderLines headerLines

-- chunkLen: current header chunk byte count
-- reqLines: previously parsed lines
-- prepend: bytestrings to be prepended

push :: Source -> Int -> ListBuilder ByteString -> (ByteString -> ByteString) -> ByteString -> IO [ByteString]
push source !chunkLen reqLines prepend bytes0 =
  case ByteString.elemIndex Byte.lf bytes0 of
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
              -- if new chunk is only LF, we need more to check for multiline
              bytes3 <- if ByteString.length bytes1 == 1 then Source.readIgnoringLeftovers1 source else pure bytes2
              push
                source
                0
                (addLine (ByteString.unsafeTake (len0 - 1) bytes0))
                id
                bytes3
        -- chunk and keep going
        False ->
          push
            source
            (chunkLen + len0)
            reqLines
            (prepend . (bytes0 <>))
            bytes1
    Just lfIndex -> do
      let bytesAfterLf = ByteString.unsafeDrop end bytes0
      case chunkLen == 0 && startsWithCrlfOrLf of
        -- Is end of headers
        True -> done bytesAfterLf
        False -> do
          -- LF is on last byte
          let p = lfIndex - 1
              chunk = if lfIndex > 0 && ByteString.unsafeIndex bytes0 p == Byte.cr then p else lfIndex
              reqLines1 = addLine (ByteString.unsafeTake chunk bytes0)
          if end == len0
            then Source.readIgnoringLeftovers1 source >>= push source 0 reqLines1 id
            else push source 0 reqLines1 id bytesAfterLf
      where
        end = lfIndex + 1
        startsWithCrlfOrLf =
          case lfIndex of
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
      pure (listBuilderBuild reqLines)

    -- addLine: take the current chunk and, if there's nothing to prepend,
    -- add straight to 'reqLines', otherwise first prepend then add.
    addLine :: ByteString -> [ByteString] -> [ByteString]
    addLine chunk =
      listBuilderAppend (if chunkLen == 0 then chunk else prepend chunk) reqLines
    {-# INLINE addLine #-}

-- Mini list builder

type ListBuilder a = [a] -> [a]

listBuilderEmpty :: ListBuilder a
listBuilderEmpty =
  id

listBuilderAppend :: a -> ListBuilder a -> ListBuilder a
listBuilderAppend x xs =
  xs . (x :)

listBuilderBuild :: ListBuilder a -> [a]
listBuilderBuild xs =
  xs []
