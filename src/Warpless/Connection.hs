module Warpless.Connection
  ( Connection (connSend, connWriteBuffer, connSendFile, connRecv),
    setConnHTTP2,
    socketConnection,
    cleanupConnection,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO.Exception (IOErrorType (InvalidArgument, ResourceVanished))
import Network.Socket qualified as Network
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Sock
import System.IO.Error (ioeGetErrorType)
import UnliftIO qualified
import Warpless.SendFile (sendFile)
import Warpless.Types (FileId, InvalidRequest (ConnectionClosedByPeer))
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer)

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection
  { -- | The sending function.
    connSend :: !(ByteString -> IO ()),
    -- | The sending function for files in HTTP/1.1.
    connSendFile :: !(FileId -> Integer -> Integer -> IO () -> [ByteString] -> IO ()),
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    connClose :: !(IO ()),
    -- | The connection receiving function. This returns "" for EOF or exceptions.
    connRecv :: !(IO ByteString),
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    connWriteBuffer :: !(IORef WriteBuffer),
    -- | Is this connection HTTP/2?
    connHTTP2 :: !(IORef Bool)
  }

setConnHTTP2 :: Connection -> IO ()
setConnHTTP2 conn =
  writeIORef (connHTTP2 conn) True

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Network.Socket -> IO Connection
socketConnection socket = do
  bufferPool <- Recv.newBufferPool 2048 16384
  connWriteBuffer <- newIORef =<< createWriteBuffer 16384
  isH2 <- newIORef False -- HTTP/1.x
  let connSend :: ByteString -> IO ()
      connSend bytes =
        Sock.sendAll socket bytes `UnliftIO.catch` \ex ->
          if ioeGetErrorType ex == ResourceVanished
            then throwIO ConnectionClosedByPeer
            else throwIO ex
  let connRecv :: IO ByteString
      connRecv =
        Recv.receive socket bufferPool `UnliftIO.catch` \ex ->
          if ioeGetErrorType ex == InvalidArgument
            then pure ByteString.empty
            else throwIO ex
  pure
    Connection
      { connSend,
        connSendFile = sendFile socket,
        connClose =
          readIORef isH2 >>= \case
            False -> Network.close socket
            True -> Network.gracefulClose socket 2000 `UnliftIO.catchAny` \_ -> pure (),
        connRecv,
        connWriteBuffer,
        connHTTP2 = isH2
      }

cleanupConnection :: Connection -> IO ()
cleanupConnection Connection {connClose, connWriteBuffer} = do
  _ <- UnliftIO.tryAny connClose
  writeBuffer <- readIORef connWriteBuffer
  bufFree writeBuffer
