-- | Byte-related utilities.
module Warpless.Byte
  ( colon,
    cr,
    lf,
    space,
    tab,
  )
where

import Data.Word (Word8)

-- | @:@
colon :: Word8
colon = 58

-- | @\\r@
cr :: Word8
cr = 13

-- | @\\n@
lf :: Word8
lf = 10

-- | @ @
space :: Word8
space = 32

-- | @\\t@
tab :: Word8
tab = 9
