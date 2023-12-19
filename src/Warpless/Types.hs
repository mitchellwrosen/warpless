module Warpless.Types
  ( Port,
    HeaderValue,
    WeirdClient (..),
    ExceptionInsideResponseBody (..),
  )
where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import UnliftIO qualified

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

----------------------------------------------------------------

-- | Exception thrown if something goes wrong while in the midst of
-- sending a response, since the status code can't be altered at that
-- point.
--
-- Used to determine whether keeping the HTTP1.1 connection / HTTP2 stream alive is safe
-- or irrecoverable.
newtype ExceptionInsideResponseBody = ExceptionInsideResponseBody UnliftIO.SomeException
  deriving stock (Show)
  deriving anyclass (Exception)
