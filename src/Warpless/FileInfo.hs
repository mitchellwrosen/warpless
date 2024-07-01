module Warpless.FileInfo
  ( FileInfo (..),
    getFileInfo,
  )
where

import GHC.Real (fromIntegral)
import Network.HTTP.Date (HTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import System.IO.Error (userError)
import System.Posix.Files
  ( fileMode,
    fileSize,
    getFileStatus,
    intersectFileModes,
    isDirectory,
    modificationTime,
    ownerReadMode,
  )
import System.Posix.Types (FileOffset)
import Warpless.Prelude

-- | File information.
data FileInfo = FileInfo
  { name :: !FilePath,
    size :: !Integer,
    -- | Modification time
    time :: !HTTPDate,
    -- | Modification time in the GMT format
    date :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Getting the file information corresponding to the file.
getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
  fs <- getFileStatus path -- file access
  let regular = not (isDirectory fs)
      readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
      time = epochTimeToHTTPDate $ modificationTime fs
  if regular && readable
    then
      pure
        FileInfo
          { name = path,
            size = fromIntegral @FileOffset @Integer (fileSize fs),
            time,
            date = formatHTTPDate time
          }
    else throwIO (userError "FileInfoCache:getInfo")
