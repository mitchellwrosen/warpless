module Warpless.Date
  ( GMTDate,
  )
where

import Data.ByteString (ByteString)

-- | The type of the Date header value.
type GMTDate = ByteString
