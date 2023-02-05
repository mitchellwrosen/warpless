module Warpless.HTTP2.File
  ( pReadMaker,
  )
where

import Data.Int (Int64)
import Network.HTTP2.Server
import Warpless.FdCache
import Warpless.SendFile (positionRead)
import Warpless.Types

-- | 'PositionReadMaker' based on file descriptor cache.
--
-- Since 3.3.13
pReadMaker :: InternalInfo -> PositionReadMaker
pReadMaker ii path = do
  (mfd, refresh) <- getFd ii path
  case mfd of
    Just fd -> return (pread fd, Refresher refresh)
    Nothing -> do
      fd <- openFile path
      return (pread fd, Closer $ closeFile fd)
  where
    pread :: Fd -> PositionRead
    pread fd off bytes buf =
      fromIntegral @Int @ByteCount
        <$> positionRead fd buf (fromIntegral @Int64 @Int bytes) (fromIntegral @Int64 @Integer off)
