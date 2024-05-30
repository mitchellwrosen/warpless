-- Adapted from `auto-update` package

module Warpless.Cached
  ( cached,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Data.Void (Void)
import Ki qualified
import Warpless.Prelude

data Value a
  = Stale {-# UNPACK #-} !(MVar a)
  | Fresh a

cached :: forall a. Ki.Scope -> IO a -> Int -> IO (IO a)
cached scope action microseconds = do
  waitingVar <- newEmptyMVar
  staleValueVar <- newEmptyMVar
  valueRef <- newIORef (Stale staleValueVar)
  Ki.fork_ scope (forever1 (recompute action microseconds waitingVar valueRef) staleValueVar)
  pure (read waitingVar valueRef)

recompute :: IO a -> Int -> MVar () -> IORef (Value a) -> MVar a -> IO (MVar a)
recompute action microseconds waitingVar valueRef staleValueVar = do
  readMVar waitingVar
  value <- action
  writeIORef valueRef (Fresh value)
  putMVar staleValueVar value
  threadDelay microseconds
  takeMVar waitingVar
  newStaleValueVar <- newEmptyMVar
  writeIORef valueRef (Stale newStaleValueVar)
  pure newStaleValueVar

read :: MVar () -> IORef (Value a) -> IO a
read waitingVar valueRef =
  readIORef valueRef >>= \case
    Fresh value -> pure value
    Stale staleValueVar -> do
      void (tryPutMVar waitingVar ())
      readMVar staleValueVar

forever1 :: forall a. (a -> IO a) -> a -> IO Void
forever1 f =
  loop
  where
    loop :: a -> IO Void
    loop x = do
      y <- f x
      loop y
