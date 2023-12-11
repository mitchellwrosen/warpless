module Warpless.ByteString
  ( readHex,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word8)

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

-- 0-9 A-F a-f
isHexDigit :: Word8 -> Bool
isHexDigit w =
  w >= 48 && w <= 57
    || w >= 65 && w <= 70
    || w >= 97 && w <= 102
