module Warpless.HTTP1.Source
  ( Source,
    make,
    read,
    readIgnoringLeftovers,
    setLeftovers,
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
make :: IO ByteString -> IO Source
make getBytes = do
  ref <- newIORef ByteString.empty
  pure (Source ref getBytes)

-- | Read a chunk of bytes.
read :: Source -> IO ByteString
read (Source ref getBytes) = do
  bytes <- readIORef ref
  if ByteString.null bytes
    then getBytes
    else do
      writeIORef ref ByteString.empty
      pure bytes

-- | Read from a Source, ignoring any leftovers.
readIgnoringLeftovers :: Source -> IO ByteString
readIgnoringLeftovers (Source _ getBytes) =
  getBytes

setLeftovers :: Source -> ByteString -> IO ()
setLeftovers (Source ref _) bytes =
  writeIORef ref bytes
