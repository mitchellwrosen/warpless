module Warpless.HTTP2.File
  ( pReadMaker,
  )
where

import Foreign.C.Error (throwErrno)
import Foreign.C.Types (CChar, CInt (CInt), CSize (CSize))
import Foreign.Ptr (Ptr, castPtr)
import GHC.Real (fromIntegral)
import Network.HTTP2.Server (PositionRead, PositionReadMaker, Sentinel (Closer))
import Network.Socket.BufferPool (Buffer)
import System.Posix (Fd, FdOption (CloseOnExec), OpenMode (ReadOnly), closeFd, defaultFileFlags, nonBlock, openFd, setFdOption)
import System.Posix.Types (ByteCount, COff (COff), CSsize (..), Fd (..), FileOffset)
import Warpless.Prelude

-- | 'PositionReadMaker' based on file descriptor cache.
--
-- Since 3.3.13
pReadMaker :: PositionReadMaker
pReadMaker path = do
  fd <- openFd path ReadOnly defaultFileFlags {nonBlock = False}
  setFdOption fd CloseOnExec True
  pure (pread fd, Closer $ closeFd fd)
  where
    pread :: Fd -> PositionRead
    pread fd off bytes buf =
      positionRead fd buf (unsafeFrom @Int64 @Word64 bytes) off

positionRead :: Fd -> Buffer -> Word64 -> Int64 -> IO Int64
positionRead fd buf siz off = do
  bytes <- c_pread fd (castPtr buf) (coerce @Word64 @CSize siz) (coerce @Int64 @COff off)
  when (bytes < 0) $ throwErrno "positionRead"
  pure (fromIntegral @CSsize @Int64 bytes)

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
