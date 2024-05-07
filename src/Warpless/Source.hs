module Warpless.Source
  ( Source,
    make,
    read,
    read1,
    readIgnoringLeftovers,
    readIgnoringLeftovers1,
    setLeftovers,
    maybeSetLeftovers,
  )
where

import Data.ByteString qualified as ByteString
import Warpless.Prelude
import Warpless.Types (WeirdClient (..))

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

-- | Read a chunk of bytes, expecting at least one byte.
read1 :: Source -> IO ByteString
read1 source = do
  bytes <- read source
  when (ByteString.null bytes) (throwIO WeirdClient)
  pure bytes

-- | Read a chunk of bytes, ignoring any leftovers.
readIgnoringLeftovers :: Source -> IO ByteString
readIgnoringLeftovers (Source _ getBytes) =
  getBytes

-- | Read a chunk of bytes, ignoring any leftovers, expecting at least one byte.
readIgnoringLeftovers1 :: Source -> IO ByteString
readIgnoringLeftovers1 source = do
  bytes <- readIgnoringLeftovers source
  when (ByteString.null bytes) (throwIO WeirdClient)
  pure bytes

setLeftovers :: Source -> ByteString -> IO ()
setLeftovers (Source ref _) bytes =
  writeIORef ref bytes

-- | Set leftovers, unless the bytes are empty.
maybeSetLeftovers :: Source -> ByteString -> IO ()
maybeSetLeftovers source bytes
  | ByteString.null bytes = pure ()
  | otherwise = setLeftovers source bytes
