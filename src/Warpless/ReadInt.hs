-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3

module Warpless.ReadInt
  ( readInt,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.Word (Word8)

{-# INLINE readInt #-}
readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs

-- This function is used to parse the Content-Length field of HTTP headers and
-- is a performance hot spot. It should only be replaced with something
-- significantly and provably faster.
--
-- It needs to be able work correctly on 32 bit CPUs for file sizes > 2G so we
-- use Int64 here and then make a generic 'readInt' that allows conversion to
-- Int and Integer.

{-# NOINLINE readInt64 #-}
readInt64 :: ByteString -> Int64
readInt64 bs =
  ByteString.foldl'
    (\ !i !c -> i * 10 + fromIntegral (c - 48))
    0
    (ByteString.takeWhile isDigit bs)

isDigit :: Word8 -> Bool
isDigit w =
  w >= 48 && w <= 57
