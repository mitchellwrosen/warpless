module Warpless.Source
  ( Source,
    mkSource,
    readSource,
    readSource',
    leftoverSource,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(IO ByteString)

mkSource :: IO ByteString -> IO Source
mkSource func = do
  ref <- newIORef S.empty
  return $! Source ref func

readSource :: Source -> IO ByteString
readSource (Source ref func) = do
  bs <- readIORef ref
  if S.null bs
    then func
    else do
      writeIORef ref S.empty
      return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bs = writeIORef ref bs
