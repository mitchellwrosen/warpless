module Warpless.HTTP2.File
  ( pReadMaker,
  )
where

import Control.Monad (when)
import Data.Int (Int64)
import Foreign.C.Error (throwErrno)
import Foreign.C.Types (CChar, CInt (CInt), CSize (CSize))
import Foreign.Ptr (Ptr, castPtr)
import Network.HTTP2.Server (PositionRead, PositionReadMaker, Sentinel (Closer))
import Network.Socket.BufferPool (BufSize, Buffer)
import System.Posix (Fd, FdOption (CloseOnExec), OpenMode (ReadOnly), closeFd, defaultFileFlags, nonBlock, openFd, setFdOption)
import System.Posix.Types (ByteCount, COff (COff), CSsize (..), Fd (..), FileOffset)

-- | 'PositionReadMaker' based on file descriptor cache.
--
-- Since 3.3.13
pReadMaker :: PositionReadMaker
pReadMaker path = do
  fd <- openFd path ReadOnly defaultFileFlags {nonBlock = False}
  setFdOption fd CloseOnExec True
  return (pread fd, Closer $ closeFd fd)
  where
    pread :: Fd -> PositionRead
    pread fd off bytes buf =
      fromIntegral @Int @Int64
        <$> positionRead fd buf (fromIntegral @Int64 @Int bytes) (fromIntegral @Int64 @Integer off)

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
  bytes <- fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
  when (bytes < 0) $ throwErrno "positionRead"
  return bytes

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
