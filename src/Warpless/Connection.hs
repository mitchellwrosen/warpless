module Warpless.Connection
  ( Connection,
    create,
    close,
    socketAddr,
    setIsHttp2,
    send,
    sendfile,
    receive,
    writeBufferRef,
  )
where

import Data.ByteString qualified as ByteString
import Network.Sendfile (FileRange (PartOfFile), sendfileWithHeader)
import Network.Socket (SockAddr, Socket, getSocketName)
import Network.Socket qualified as Network
import Network.Socket.BufferPool (BufferPool)
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Sock
import Warpless.Exception (ignoringExceptions, isSyncException)
import Warpless.Prelude
import Warpless.Types (WeirdClient (..))
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer, freeWriteBuffer)

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection
  { -- | The underlying socket.
    connSock :: !Socket,
    socketAddr :: !SockAddr,
    bufferPool :: !BufferPool,
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    writeBufferRef :: !(IORef WriteBuffer),
    -- | Is this connection HTTP/2?
    connHTTP2 :: !(IORef Bool)
  }

-- | Creating 'Connection' for plain HTTP based on a given socket.
create :: Network.Socket -> IO Connection
create socket = do
  socketAddr <- getSocketName socket
  bufferPool <- Recv.newBufferPool 2048 16384
  writeBuffer <- createWriteBuffer 16384
  writeBufferRef <- newIORef writeBuffer
  isHttp2 <- newIORef False -- HTTP/1.x
  pure
    Connection
      { connSock = socket,
        socketAddr,
        bufferPool,
        writeBufferRef,
        connHTTP2 = isHttp2
      }

-- | Clean up a connection. Never throws an exception.
--
-- Precondition: called with exceptions uninterruptibly masked.
close :: Connection -> IO ()
close Connection {connHTTP2, connSock, writeBufferRef} = do
  readIORef connHTTP2 >>= \case
    False -> Network.close connSock -- doesn't throw
    True -> ignoringExceptions (Network.gracefulClose connSock 2000)
  writeBuffer <- readIORef writeBufferRef
  freeWriteBuffer writeBuffer

setIsHttp2 :: Connection -> IO ()
setIsHttp2 conn =
  writeIORef (connHTTP2 conn) True

-- | The connection sending function.
--
-- If the inner socket sending function throws an exception, it is re-thrown as a WeirdClient (thus blaming the client
-- for anything that might happen during trying to send it bytes, even if it's not the client's fault). This is because
-- we don't really want to change our behavior per any particular exception type - we just consider this connection
-- busted and move on.
send :: Connection -> ByteString -> IO ()
send Connection {connSock} bytes =
  Sock.sendAll connSock bytes `catch` \exception ->
    if isSyncException exception
      then throwIO WeirdClient
      else throwIO exception

-- | The sending function for files in HTTP/1.1.
sendfile :: Connection -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendfile Connection {connSock} path off len =
  sendfileWithHeader connSock path (PartOfFile off len)

-- | The connection receiving function.
--
-- This function never throws an exception; synchronous exceptions are returned as empty bytes.
receive :: Connection -> IO ByteString
receive Connection {connSock, bufferPool} =
  Recv.receive connSock bufferPool `catch` \exception ->
    if isSyncException exception
      then pure ByteString.empty
      else throwIO exception
