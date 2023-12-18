-- | Byte-related utilities.
module Warpless.Byte
  ( cr,
    newline,
  )
where

import Data.Word (Word8)

-- | @\\r@
cr :: Word8
cr = 13

-- | @\\n@
newline :: Word8
newline = 10
