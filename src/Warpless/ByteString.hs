module Warpless.ByteString
  ( containsNewlines,
    readHex,
    readInt64,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.Word (Word8)
import Warpless.Byte qualified as Byte

containsNewlines :: ByteString -> Bool
containsNewlines =
  ByteString.any (\w -> w == Byte.cr || w == Byte.lf)
{-# INLINE containsNewlines #-}

-- Read a hex number from the head of bytes, and ignore the rest:
--
-- >>> readHex "FF__ignored__garbage"
-- 255
readHex :: ByteString -> Word
readHex =
  ByteString.foldl' (\i c -> i * 16 + fromIntegral @Word8 @Word (hexToWord c)) 0
    . ByteString.takeWhile isHexDigit

-- hexToWord 0-9 = 0-9
-- hexToWord A-F = 10-15
-- hexToWord a-f = 10-15
hexToWord :: Word8 -> Word8
hexToWord w
  | w < 58 = w - 48
  | w < 71 = w - 55
  | otherwise = w - 87

-- Read a hex number from the head of bytes, and ignore the rest:
--
-- >>> readInt64 "42__ignored__garbage"
-- 42
readInt64 :: ByteString -> Int64
readInt64 =
  ByteString.foldl' (\i c -> i * 10 + fromIntegral @Word8 @Int64 c - 48) 0
    . ByteString.takeWhile isDigit
{-# INLINE readInt64 #-}

-- 0-9
isDigit :: Word8 -> Bool
isDigit w =
  w >= 48 && w <= 57
{-# INLINE isDigit #-}

-- 0-9 A-F a-f
isHexDigit :: Word8 -> Bool
isHexDigit w =
  isDigit w
    || w >= 65 && w <= 70
    || w >= 97 && w <= 102
{-# INLINE isHexDigit #-}
