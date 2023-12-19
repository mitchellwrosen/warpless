-- | Byte-related utilities.
module Warpless.Byte
  ( cr,
    lf,
  )
where

import Data.Word (Word8)

-- | @\\r@
cr :: Word8
cr = 13

-- | @\\n@
lf :: Word8
lf = 10
