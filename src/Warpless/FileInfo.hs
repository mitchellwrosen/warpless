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
  { fileInfoName :: !FilePath,
    fileInfoSize :: !Integer,
    -- | Modification time
    fileInfoTime :: !HTTPDate,
    -- | Modification time in the GMT format
    fileInfoDate :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Getting the file information corresponding to the file.
getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
  fs <- getFileStatus path -- file access
  let regular = not (isDirectory fs)
      readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
  if regular && readable
    then do
      let time = epochTimeToHTTPDate $ modificationTime fs
          date = formatHTTPDate time
          size = fileSize fs
          info =
            FileInfo
              { fileInfoName = path,
                fileInfoSize = fromIntegral @FileOffset @Integer size,
                fileInfoTime = time,
                fileInfoDate = date
              }
      pure info
    else throwIO (userError "FileInfoCache:getInfo")
