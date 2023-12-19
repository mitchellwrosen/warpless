module Warpless.Connection
  ( Connection,
    setConnHTTP2,
    socketConnection,
    connSend,
    connSendFile,
    connRecv,
    connWriteBuffer,
    connMySockAddr,
    cleanupConnection,
  )
where

import Control.Exception (catch, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO.Exception (IOErrorType (InvalidArgument, ResourceVanished))
import Network.Sendfile (FileRange (PartOfFile), sendfileWithHeader)
import Network.Socket (SockAddr, Socket, getSocketName)
import Network.Socket qualified as Network
import Network.Socket.BufferPool (BufferPool)
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Sock
import System.IO.Error (ioeGetErrorType)
import Warpless.Exception (ignoringExceptions)
import Warpless.Types (WeirdClient (..))
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer, freeWriteBuffer)

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection
  { -- | The underlying socket.
    connSock :: !Socket,
    bufferPool :: !BufferPool,
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    connClose :: !(IO ()),
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    connWriteBuffer :: !(IORef WriteBuffer),
    -- | Is this connection HTTP/2?
    connHTTP2 :: !(IORef Bool),
    connMySockAddr :: !SockAddr
  }

setConnHTTP2 :: Connection -> IO ()
setConnHTTP2 conn =
  writeIORef (connHTTP2 conn) True

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Network.Socket -> IO Connection
socketConnection socket = do
  bufferPool <- Recv.newBufferPool 2048 16384
  connWriteBuffer <- newIORef =<< createWriteBuffer 16384
  isHttp2 <- newIORef False -- HTTP/1.x
  mySockAddr <- getSocketName socket
  pure
    Connection
      { connSock = socket,
        bufferPool,
        connClose =
          readIORef isHttp2 >>= \case
            False -> Network.close socket -- doesn't throw
            True -> ignoringExceptions (Network.gracefulClose socket 2000),
        connWriteBuffer,
        connHTTP2 = isHttp2,
        connMySockAddr = mySockAddr
      }

connSend :: Connection -> ByteString -> IO ()
connSend Connection {connSock} bytes =
  Sock.sendAll connSock bytes `catch` \(ex :: IOError) ->
    if ioeGetErrorType ex == ResourceVanished
      then throwIO WeirdClient
      else throwIO ex

-- | The sending function for files in HTTP/1.1.
connSendFile :: Connection -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
connSendFile Connection {connSock} path off len =
  sendfileWithHeader connSock path (PartOfFile off len)

-- | The connection receiving function. This returns "" for EOF.
connRecv :: Connection -> IO ByteString
connRecv Connection {connSock, bufferPool} =
  Recv.receive connSock bufferPool `catch` \(ex :: IOError) ->
    if ioeGetErrorType ex == InvalidArgument
      then pure ByteString.empty
      else throwIO ex

-- | Clean up a connection. Never throws an exception.
--
-- Precondition: called with exceptions uninterruptibly masked.
cleanupConnection :: Connection -> IO ()
cleanupConnection Connection {connClose, connWriteBuffer} = do
  connClose
  writeBuffer <- readIORef connWriteBuffer
  freeWriteBuffer writeBuffer
