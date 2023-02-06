module Warpless.Counter
  ( Counter,
    newCounter,
    waitForZero,
    increase,
    decrease,
  )
where

import Control.Concurrent.STM
import Control.Monad (when)

newtype Counter = Counter (TVar Int)

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

waitForZero :: Counter -> IO ()
waitForZero (Counter ref) = atomically $ do
  x <- readTVar ref
  when (x /= 0) retry

increase :: Counter -> IO ()
increase (Counter ref) = atomically $ modifyTVar' ref $ \x -> x + 1

decrease :: Counter -> IO ()
decrease (Counter ref) = atomically $ modifyTVar' ref $ \x -> x - 1
