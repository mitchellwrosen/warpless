module Warpless.Connection
  ( Connection (..),
    create,
    close,
    send,
    sendBuilder,
    sendfile,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (BufferWriter, Next (Chunk, Done, More), runBuilder)
import Data.ByteString.Internal (ByteString (PS))
import Foreign (Ptr, free, newForeignPtr_)
import Network.Sendfile (FileRange (PartOfFile), sendfileWithHeader)
import Network.Socket qualified as Network
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Network
import Warpless.Exception (isSyncException)
import Warpless.Prelude
import Warpless.Source (Source)
import Warpless.Source qualified as Source
import Warpless.Types (WeirdClient (..))
import Warpless.WriteBuffer (WriteBuffer (..))
import Warpless.WriteBuffer qualified as WriteBuffer

data Connection = Connection
  { -- | The underlying socket.
    socket :: !Network.Socket,
    sockAddr :: !Network.SockAddr,
    source :: !Source,
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    writeBufferRef :: !(IORef WriteBuffer)
  }

-- | Creating 'Connection' for plain HTTP based on a given socket.
create :: Network.Socket -> Network.SockAddr -> IO Connection
create socket sockAddr = do
  bufferPool <- Recv.newBufferPool 2048 16384
  source <-
    Source.make do
      Recv.receive socket bufferPool `catch` \exception ->
        if isSyncException exception
          then pure ByteString.empty
          else throwIO exception
  writeBuffer <- WriteBuffer.allocate 16384
  writeBufferRef <- newIORef writeBuffer
  pure Connection {socket, sockAddr, source, writeBufferRef}

-- | Clean up a connection. Never throws an exception.
--
-- Precondition: called with exceptions uninterruptibly masked.
close :: Connection -> IO ()
close conn = do
  Network.close conn.socket -- doesn't throw
  WriteBuffer buf _size <- readIORef conn.writeBufferRef
  free buf

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

sendBuilder :: Connection -> Builder -> IO ()
sendBuilder conn = \builder -> do
  WriteBuffer buf size <- readIORef conn.writeBufferRef
  loop buf size (runBuilder builder)
  where
    loop :: Ptr Word8 -> Int -> BufferWriter -> IO ()
    loop buf size writer = do
      (len, signal) <- writer buf size
      fptr <- newForeignPtr_ buf
      send conn (PS fptr 0 len)
      case signal of
        Done -> pure ()
        More minSize writer1
          | size < minSize -> do
              WriteBuffer buf1 size1 <-
                mask_ do
                  free buf
                  biggerWriteBuffer <- WriteBuffer.allocate minSize
                  writeIORef conn.writeBufferRef biggerWriteBuffer
                  pure biggerWriteBuffer
              loop buf1 size1 writer1
          | otherwise -> loop buf size writer1
        Chunk bytes writer1 -> do
          send conn bytes
          loop buf size writer1

-- | The sending function for files in HTTP/1.1.
sendfile :: Connection -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendfile conn path off len =
  sendfileWithHeader conn.socket path (PartOfFile off len)
