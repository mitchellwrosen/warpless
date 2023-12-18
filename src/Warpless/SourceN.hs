module Warpless.SourceN
  ( new,
    read,
  )
where

import Control.Exception (throwIO)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef
-- import Warpless (InvalidRequest (ConnectionClosedByPeer))
import Warpless.Source
import Warpless.Types (InvalidRequest (ConnectionClosedByPeer))
import Prelude hiding (read)

data SourceN
  = SourceN
      {-# UNPACK #-} !Source
      {-# UNPACK #-} !(IORef Int)

new :: Source -> Int -> IO SourceN
new source remaining_ = do
  remainingRef <- newIORef remaining_
  pure (SourceN source remainingRef)

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
read :: SourceN -> IO ByteString
read (SourceN source remainingRef) = do
  readIORef remainingRef >>= \case
    0 -> pure ByteString.empty
    remaining_ -> do
      bytes <- readSource source
      when (ByteString.null bytes) (throwIO ConnectionClosedByPeer) -- client didn't send enough bytes
      let count = min remaining_ (ByteString.length bytes)
      let nextRemaining = remaining_ - count
      writeIORef remainingRef nextRemaining
      if nextRemaining > 0
        then pure bytes
        else do
          let (bytes1, leftovers) = ByteString.splitAt count bytes
          leftoverSource source leftovers
          pure bytes1
