module Warpless.Connection
  ( Connection,
    create,
    close,
    setIsHttp2,
    send,
    sendfile,
    receive,
    connWriteBuffer,
    connMySockAddr,
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
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    connWriteBuffer :: !(IORef WriteBuffer),
    -- | Is this connection HTTP/2?
    connHTTP2 :: !(IORef Bool),
    connMySockAddr :: !SockAddr
  }

-- | Creating 'Connection' for plain HTTP based on a given socket.
create :: Network.Socket -> IO Connection
create socket = do
  bufferPool <- Recv.newBufferPool 2048 16384
  connWriteBuffer <- newIORef =<< createWriteBuffer 16384
  isHttp2 <- newIORef False -- HTTP/1.x
  mySockAddr <- getSocketName socket
  pure
    Connection
      { connSock = socket,
        bufferPool,
        connWriteBuffer,
        connHTTP2 = isHttp2,
        connMySockAddr = mySockAddr
      }

-- | Clean up a connection. Never throws an exception.
--
-- Precondition: called with exceptions uninterruptibly masked.
close :: Connection -> IO ()
close Connection {connHTTP2, connSock, connWriteBuffer} = do
  readIORef connHTTP2 >>= \case
    False -> Network.close connSock -- doesn't throw
    True -> ignoringExceptions (Network.gracefulClose connSock 2000)
  writeBuffer <- readIORef connWriteBuffer
  freeWriteBuffer writeBuffer

setIsHttp2 :: Connection -> IO ()
setIsHttp2 conn =
  writeIORef (connHTTP2 conn) True

send :: Connection -> ByteString -> IO ()
send Connection {connSock} bytes =
  Sock.sendAll connSock bytes `catch` \(ex :: IOError) ->
    if ioeGetErrorType ex == ResourceVanished
      then throwIO WeirdClient
      else throwIO ex

-- | The sending function for files in HTTP/1.1.
sendfile :: Connection -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendfile Connection {connSock} path off len =
  sendfileWithHeader connSock path (PartOfFile off len)

-- | The connection receiving function. This returns "" for EOF.
receive :: Connection -> IO ByteString
receive Connection {connSock, bufferPool} =
  Recv.receive connSock bufferPool `catch` \(ex :: IOError) ->
    if ioeGetErrorType ex == InvalidArgument
      then pure ByteString.empty
      else throwIO ex
