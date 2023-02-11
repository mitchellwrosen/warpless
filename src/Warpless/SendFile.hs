module Warpless.SendFile
  ( sendFile,
    positionRead,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Foreign.C.Error (throwErrno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import Network.Sendfile
import Network.Socket (Socket)
import Network.Socket.BufferPool
import System.Posix.Types
import Warpless.Types

----------------------------------------------------------------

-- | Function to send a file based on sendfile() for Linux\/Mac\/FreeBSD.
--   This makes use of the file descriptor cache.
--   For other OSes, this is identical to 'readSendFile'.
--
-- Since: 3.1.0
sendFile :: Socket -> FileId -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendFile s fid off len act hdr =
  case fileIdFd fid of
    -- settingsFdCacheDuration is 0
    Nothing -> sendfileWithHeader s (fileIdPath fid) (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd (PartOfFile off len) act hdr

----------------------------------------------------------------

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
  bytes <- fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
  when (bytes < 0) $ throwErrno "positionRead"
  return bytes

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
