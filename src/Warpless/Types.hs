module Warpless.Types
  ( Port,
    HeaderValue,
    WeirdClient (..),
  )
where

import Warpless.Prelude

----------------------------------------------------------------

-- | TCP port number.
type Port = Int

----------------------------------------------------------------

-- | The type for header value used with 'HeaderName'.
type HeaderValue = ByteString

----------------------------------------------------------------

-- | The client either closed the connection, or sent some data we couldn't parse, or didn't send enough data. Whatever
-- the case, we simply want to stop servicing this client and close the connection.
data WeirdClient
  = WeirdClient
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
