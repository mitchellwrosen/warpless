module Warpless.HTTP2.File
  ( pReadMaker,
  )
where

import Data.Int (Int64)
import Network.HTTP2.Server
import System.Posix (Fd, FdOption (CloseOnExec), OpenMode (ReadOnly), closeFd, defaultFileFlags, nonBlock, openFd, setFdOption)
import Warpless.SendFile (positionRead)

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
      fromIntegral @Int @ByteCount
        <$> positionRead fd buf (fromIntegral @Int64 @Int bytes) (fromIntegral @Int64 @Integer off)
