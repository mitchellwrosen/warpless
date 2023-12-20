module Warpless.Source
  ( Source,
    mkSource,
    readSource,
    readSource',
    leftoverSource,
  )
where

import Data.ByteString qualified as ByteString
import Warpless.Prelude

-- | A source of bytes.
data Source
  = Source
      {-# UNPACK #-} !(IORef ByteString)
      !(IO ByteString)

-- | Make a source of bytes from an IO action that either:
--
-- * Returns one or more bytes, and can be run again.
-- * Returns zero bytes, indicating that there are no more bytes.
mkSource :: IO ByteString -> IO Source
mkSource getBytes = do
  ref <- newIORef ByteString.empty
  pure (Source ref getBytes)

-- | Read a chunk of bytes.
readSource :: Source -> IO ByteString
readSource (Source ref getBytes) = do
  bytes <- readIORef ref
  if ByteString.null bytes
    then getBytes
    else do
      writeIORef ref ByteString.empty
      pure bytes

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ getBytes) =
  getBytes

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bytes =
  writeIORef ref bytes
