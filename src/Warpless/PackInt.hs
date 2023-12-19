module Warpless.PackInt
  ( packInteger,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (unsafeCreate)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

packInteger :: Integer -> ByteString
packInteger 0 = "0"
packInteger n | n < 0 = error "packIntegral"
packInteger n = unsafeCreate len go0
  where
    n' = fromIntegral n + 1 :: Double
    len = ceiling $ logBase 10 n' :: Int
    go0 :: Ptr Word8 -> IO ()
    go0 p = go n $ p `plusPtr` (len - 1)
    go :: (Integral a) => a -> Ptr Word8 -> IO ()
    go i p = do
      let (d, r) = i `divMod` 10
      poke p (48 + fromIntegral r)
      when (d /= 0) $ go d (p `plusPtr` (-1))
