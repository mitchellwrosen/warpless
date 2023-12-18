module Warpless.Types
  ( Port,
    HeaderValue,
    InvalidRequest (..),
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

-- | Error types for bad 'Request'.
data InvalidRequest
  = NotEnoughLines ![String]
  | BadFirstLine !String
  | NonHttp
  | IncompleteHeaders
  | ConnectionClosedByPeer
  deriving stock (Eq)
  deriving anyclass (Exception)

instance Show InvalidRequest where
  show (NotEnoughLines xs) = "Warp: Incomplete request headers, received: " ++ show xs
  show (BadFirstLine s) = "Warp: Invalid first line of request: " ++ show s
  show NonHttp = "Warp: Request line specified a non-HTTP request"
  show IncompleteHeaders = "Warp: Request headers did not finish transmission"
  show ConnectionClosedByPeer = "Warp: Client closed connection prematurely"

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
