module Warpless.Connection
  ( Connection (..),
    setConnHTTP2,
  )
where

import Data.ByteString (ByteString)
import Data.IORef (IORef, writeIORef)
import Network.Socket.BufferPool
import Warpless.Types

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
    -- | Obsoleted.
    connRecvBuf :: !(Buffer -> BufSize -> IO Bool),
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
