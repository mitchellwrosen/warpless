-- | File descriptor cache to avoid locks in kernel.
module Warpless.FdCache
  ( withFdCache,
    Fd,
    Refresh,
    openFile,
    closeFile,
    setFileCloseOnExec,
  )
where

import Control.Reaper
import Data.IORef
import System.Posix.IO (FdOption (CloseOnExec), OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd, setFdOption)
import System.Posix.Types (Fd)
import UnliftIO.Exception (bracket)
import Warpless.MultiMap as MM

----------------------------------------------------------------

-- | An action to activate a Fd cache entry.
type Refresh = IO ()

getFdNothing :: FilePath -> IO (Maybe Fd, Refresh)
getFdNothing _ = return (Nothing, return ())

----------------------------------------------------------------

-- | Creating 'MutableFdCache' and executing the action in the second
--   argument. The first argument is a cache duration in second.
withFdCache :: Int -> ((FilePath -> IO (Maybe Fd, Refresh)) -> IO a) -> IO a
withFdCache duration action
  | duration == 0 = action getFdNothing
  | otherwise = bracket (initialize duration) terminate (action . getFd)

----------------------------------------------------------------

data Status = Active | Inactive

newtype MutableStatus = MutableStatus (IORef Status)

status :: MutableStatus -> IO Status
status (MutableStatus ref) = readIORef ref

newActiveStatus :: IO MutableStatus
newActiveStatus = MutableStatus <$> newIORef Active

refresh :: MutableStatus -> Refresh
refresh (MutableStatus ref) = writeIORef ref Active

inactive :: MutableStatus -> IO ()
inactive (MutableStatus ref) = writeIORef ref Inactive

----------------------------------------------------------------

data FdEntry = FdEntry !Fd !MutableStatus

openFile :: FilePath -> IO Fd
openFile path = do
  fd <- openFd path ReadOnly defaultFileFlags {nonBlock = False}
  setFileCloseOnExec fd
  return fd

closeFile :: Fd -> IO ()
closeFile = closeFd

newFdEntry :: FilePath -> IO FdEntry
newFdEntry path = FdEntry <$> openFile path <*> newActiveStatus

setFileCloseOnExec :: Fd -> IO ()
setFileCloseOnExec fd = setFdOption fd CloseOnExec True

----------------------------------------------------------------

type FdCache = MultiMap FdEntry

-- | Mutable Fd cacher.
newtype MutableFdCache = MutableFdCache (Reaper FdCache (FilePath, FdEntry))

fdCache :: MutableFdCache -> IO FdCache
fdCache (MutableFdCache reaper) = reaperRead reaper

look :: MutableFdCache -> FilePath -> IO (Maybe FdEntry)
look mfc path = MM.lookup path <$> fdCache mfc

----------------------------------------------------------------

-- The first argument is a cache duration in second.
initialize :: Int -> IO MutableFdCache
initialize duration = MutableFdCache <$> mkReaper settings
  where
    settings =
      defaultReaperSettings
        { reaperAction = clean,
          reaperDelay = duration,
          reaperCons = uncurry insert,
          reaperNull = isEmpty,
          reaperEmpty = empty
        }

clean :: FdCache -> IO (FdCache -> FdCache)
clean old = do
  new <- pruneWith old prune
  return $ merge new
  where
    prune :: (FilePath, FdEntry) -> IO Bool
    prune (_, FdEntry fd mst) = status mst >>= act
      where
        act Active = inactive mst >> return True
        act Inactive = closeFd fd >> return False

----------------------------------------------------------------

terminate :: MutableFdCache -> IO ()
terminate (MutableFdCache reaper) = do
  !t <- reaperStop reaper
  mapM_ (closeIt . snd) $ toList t
  where
    closeIt (FdEntry fd _) = closeFd fd

----------------------------------------------------------------

-- | Getting 'Fd' and 'Refresh' from the mutable Fd cacher.
getFd :: MutableFdCache -> FilePath -> IO (Maybe Fd, Refresh)
getFd mfc@(MutableFdCache reaper) path = look mfc path >>= get
  where
    get Nothing = do
      ent@(FdEntry fd mst) <- newFdEntry path
      reaperAdd reaper (path, ent)
      return (Just fd, refresh mst)
    get (Just (FdEntry fd mst)) = do
      refresh mst
      return (Just fd, refresh mst)
