module Warpless.Types
  ( Port,
    HeaderValue,
    InvalidRequest (..),
    ExceptionInsideResponseBody (..),
    FileId (..),
    InternalInfo (..),
  )
where

import Data.ByteString (ByteString)
import System.Posix.Types (Fd)
import UnliftIO qualified
import Warpless.Date qualified as D
import Warpless.FdCache qualified as F
import Warpless.FileInfoCache qualified as I

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
  | OverLargeHeader
  | BadProxyHeader !String
  | -- | Since 3.3.22
    PayloadTooLarge
  | -- | Since 3.3.22
    RequestHeaderFieldsTooLarge
  deriving stock (Eq)

instance Show InvalidRequest where
  show (NotEnoughLines xs) = "Warp: Incomplete request headers, received: " ++ show xs
  show (BadFirstLine s) = "Warp: Invalid first line of request: " ++ show s
  show NonHttp = "Warp: Request line specified a non-HTTP request"
  show IncompleteHeaders = "Warp: Request headers did not finish transmission"
  show ConnectionClosedByPeer = "Warp: Client closed connection prematurely"
  show OverLargeHeader = "Warp: Request headers too large, possible memory attack detected. Closing connection."
  show (BadProxyHeader s) = "Warp: Invalid PROXY protocol header: " ++ show s
  show RequestHeaderFieldsTooLarge = "Request header fields too large"
  show PayloadTooLarge = "Payload too large"

instance UnliftIO.Exception InvalidRequest

----------------------------------------------------------------

-- | Exception thrown if something goes wrong while in the midst of
-- sending a response, since the status code can't be altered at that
-- point.
--
-- Used to determine whether keeping the HTTP1.1 connection / HTTP2 stream alive is safe
-- or irrecoverable.
newtype ExceptionInsideResponseBody = ExceptionInsideResponseBody UnliftIO.SomeException
  deriving stock (Show)

instance UnliftIO.Exception ExceptionInsideResponseBody

----------------------------------------------------------------

-- | Data type to abstract file identifiers.
--   On Unix, a file descriptor would be specified to make use of
--   the file descriptor cache.
--
-- Since: 3.1.0
data FileId = FileId
  { fileIdPath :: !FilePath,
    fileIdFd :: !(Maybe Fd)
  }

----------------------------------------------------------------

data InternalInfo = InternalInfo
  { getDate :: !(IO D.GMTDate),
    getFd :: !(FilePath -> IO (Maybe F.Fd, F.Refresh)),
    getFileInfo :: !(FilePath -> IO I.FileInfo)
  }
