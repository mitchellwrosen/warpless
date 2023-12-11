module Warpless.Source
  ( Source,
    mkSource,
    readSource,
    readSource',
    leftoverSource,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A source of bytes.
data Source
  = Source !(IORef ByteString) !(IO ByteString)

-- | Make a source of bytes from an IO action that either:
--
-- * Returns one or more bytes, and can be run again.
-- * Returns zero bytes, indicating that there are no more bytes.
mkSource :: IO ByteString -> IO Source
mkSource func = do
  ref <- newIORef ByteString.empty
  pure (Source ref func)

-- | Read a chunk of bytes.
readSource :: Source -> IO ByteString
readSource (Source ref func) = do
  bs <- readIORef ref
  if ByteString.null bs
    then func
    else do
      writeIORef ref ByteString.empty
      return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bs =
  writeIORef ref bs
