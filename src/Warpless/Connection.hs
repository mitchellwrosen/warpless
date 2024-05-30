module Warpless.Connection
  ( Connection,
    create,
    close,
    send,
    sendfile,
    receive,
    writeBufferRef,
  )
where

import Data.ByteString qualified as ByteString
import Network.Sendfile (FileRange (PartOfFile), sendfileWithHeader)
import Network.Socket qualified as Network
import Network.Socket.BufferPool (BufferPool)
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Network
import Warpless.Exception (isSyncException)
import Warpless.Prelude
import Warpless.Types (WeirdClient (..))
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer, freeWriteBuffer)

data Connection = Connection
  { -- | The underlying socket.
    socket :: !Network.Socket,
    bufferPool :: !BufferPool,
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    writeBufferRef :: !(IORef WriteBuffer)
  }

-- | Creating 'Connection' for plain HTTP based on a given socket.
create :: Network.Socket -> IO Connection
create socket = do
  bufferPool <- Recv.newBufferPool 2048 16384
  writeBuffer <- createWriteBuffer 16384
  writeBufferRef <- newIORef writeBuffer
  pure Connection {socket, bufferPool, writeBufferRef}

-- | Clean up a connection. Never throws an exception.
--
-- Precondition: called with exceptions uninterruptibly masked.
close :: Connection -> IO ()
close conn = do
  Network.close conn.socket -- doesn't throw
  writeBuffer <- readIORef conn.writeBufferRef
  freeWriteBuffer writeBuffer

-- | The connection sending function.
--
-- If the inner socket sending function throws an exception, it is re-thrown as a WeirdClient (thus blaming the client
-- for anything that might happen during trying to send it bytes, even if it's not the client's fault). This is because
-- we don't really want to change our behavior per any particular exception type - we just consider this connection
-- busted and move on.
send :: Connection -> ByteString -> IO ()
send conn bytes =
  Network.sendAll conn.socket bytes `catch` \exception ->
    if isSyncException exception
      then throwIO WeirdClient
      else throwIO exception

-- | The sending function for files in HTTP/1.1.
sendfile :: Connection -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendfile conn path off len =
  sendfileWithHeader conn.socket path (PartOfFile off len)

-- | The connection receiving function.
--
-- This function never throws an exception; synchronous exceptions are returned as empty bytes.
receive :: Connection -> IO ByteString
receive conn =
  Recv.receive conn.socket conn.bufferPool `catch` \exception ->
    if isSyncException exception
      then pure ByteString.empty
      else throwIO exception
