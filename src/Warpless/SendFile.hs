module Warpless.SendFile
  ( positionRead,
  )
where

import Control.Monad (when)
import Foreign.C.Error (throwErrno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import Network.Socket.BufferPool (BufSize, Buffer)
import System.Posix.Types

----------------------------------------------------------------

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
  bytes <- fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
  when (bytes < 0) $ throwErrno "positionRead"
  return bytes

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
