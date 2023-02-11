module Warpless.Connection
  ( Connection (..),
    setConnHTTP2,
    socketConnection,
    cleanupConnection,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket qualified as Network
import Network.Socket.BufferPool
import Network.Socket.ByteString qualified as Sock
import System.IO.Error (ioeGetErrorType)
import UnliftIO qualified
import Warpless.SendFile
import Warpless.Types
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer)

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection
  { -- | The sending function.
    connSendAll :: !(ByteString -> IO ()),
    -- | The sending function for files in HTTP/1.1.
    connSendFile :: !SendFile,
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    connClose :: !(IO ()),
    -- | The connection receiving function. This returns "" for EOF or exceptions.
    connRecv :: !Recv,
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

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Network.Socket -> IO Connection
socketConnection s = do
  bufferPool <- newBufferPool 2048 16384
  writeBuffer <- createWriteBuffer 16384
  writeBufferRef <- newIORef writeBuffer
  isH2 <- newIORef False -- HTTP/1.x
  return
    Connection
      { connSendAll = sendall,
        connSendFile = sendfile writeBufferRef,
        connClose =
          readIORef isH2 >>= \case
            False -> Network.close s
            True -> Network.gracefulClose s 2000 `UnliftIO.catchAny` \_ -> pure (),
        connRecv = receive' s bufferPool,
        connWriteBuffer = writeBufferRef,
        connHTTP2 = isH2
      }
  where
    receive' sock pool = UnliftIO.handleIO handler $ receive sock pool
      where
        handler :: IOException -> IO ByteString
        handler e
          | ioeGetErrorType e == InvalidArgument = return ""
          | otherwise = throwIO e

    sendfile writeBufferRef fid offset len hook headers = do
      writeBuffer <- readIORef writeBufferRef
      sendFile
        s
        (bufBuffer writeBuffer)
        (bufSize writeBuffer)
        sendall
        fid
        offset
        len
        hook
        headers

    sendall :: ByteString -> IO ()
    sendall bytes =
      UnliftIO.handleJust
        ( \e ->
            if ioeGetErrorType e == ResourceVanished
              then Just ConnectionClosedByPeer
              else Nothing
        )
        UnliftIO.throwIO
        $ Sock.sendAll s bytes

cleanupConnection :: Connection -> IO ()
cleanupConnection Connection {connClose, connWriteBuffer} = do
  _ <- UnliftIO.tryAny connClose
  writeBuffer <- readIORef connWriteBuffer
  bufFree writeBuffer
