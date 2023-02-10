module Warpless.Types
  ( Port,
    HeaderValue,
    InvalidRequest (..),
    ExceptionInsideResponseBody (..),
    FileId (..),
    SendFile,
    WriteBuffer (..),
    Connection (..),
    setConnHTTP2,
    InternalInfo (..),
    Source,
    mkSource,
    readSource,
    readSource',
    leftoverSource,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket.BufferPool
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

-- |  fileid, offset, length, hook action, HTTP headers
--
-- Since: 3.1.0
type SendFile = FileId -> Integer -> Integer -> IO () -> [ByteString] -> IO ()

-- | A write buffer of a specified size
-- containing bytes and a way to free the buffer.
data WriteBuffer = WriteBuffer
  { bufBuffer :: !Buffer,
    -- | The size of the write buffer.
    bufSize :: !BufSize,
    -- | Free the allocated buffer. Warp guarantees it will only be
    -- called once, and no other functions will be called after it.
    bufFree :: !(IO ())
  }

type RecvBuf = Buffer -> BufSize -> IO Bool

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection
  { -- | This is not used at this moment.
    connSendMany :: !([ByteString] -> IO ()),
    -- | The sending function.
    connSendAll :: !(ByteString -> IO ()),
    -- | The sending function for files in HTTP/1.1.
    connSendFile :: !SendFile,
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    connClose :: !(IO ()),
    -- | The connection receiving function. This returns "" for EOF or exceptions.
    connRecv :: !Recv,
    -- | Obsoleted.
    connRecvBuf :: !RecvBuf,
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    connWriteBuffer :: !(IORef WriteBuffer),
    -- | Is this connection HTTP/2?
    connHTTP2 :: !(IORef Bool)
  }

setConnHTTP2 :: Connection -> Bool -> IO ()
setConnHTTP2 conn b = writeIORef (connHTTP2 conn) b

----------------------------------------------------------------

data InternalInfo = InternalInfo
  { getDate :: !(IO D.GMTDate),
    getFd :: !(FilePath -> IO (Maybe F.Fd, F.Refresh)),
    getFileInfo :: !(FilePath -> IO I.FileInfo)
  }

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(IO ByteString)

mkSource :: IO ByteString -> IO Source
mkSource func = do
  ref <- newIORef S.empty
  return $! Source ref func

readSource :: Source -> IO ByteString
readSource (Source ref func) = do
  bs <- readIORef ref
  if S.null bs
    then func
    else do
      writeIORef ref S.empty
      return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bs = writeIORef ref bs
