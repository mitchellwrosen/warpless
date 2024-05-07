module Warpless.SourceN
  ( new,
    read,
  )
where

import Data.ByteString qualified as ByteString
import Warpless.Prelude
import Warpless.Source (Source)
import Warpless.Source qualified as Source

data SourceN
  = SourceN
      {-# UNPACK #-} !Source
      {-# UNPACK #-} !(IORef Int)

new :: Source -> Int -> IO SourceN
new source remaining_ = do
  remainingRef <- newIORef remaining_
  pure (SourceN source remainingRef)

read :: SourceN -> IO ByteString
read (SourceN source remainingRef) = do
  readIORef remainingRef >>= \case
    0 -> pure ByteString.empty
    remaining_ -> do
      bytes <- Source.read1 source
      let count = min remaining_ (ByteString.length bytes)
      let nextRemaining = remaining_ - count
      writeIORef remainingRef nextRemaining
      if nextRemaining > 0
        then pure bytes
        else do
          let (bytes1, leftovers) = ByteString.splitAt count bytes
          Source.setLeftovers source leftovers
          pure bytes1
