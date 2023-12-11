-- | Exception utils.
module Warpless.Exception
  ( ignoringExceptions,
  )
where

import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)

ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catch` \(exception :: SomeException) ->
    case fromException @SomeAsyncException exception of
      Nothing -> pure ()
      Just _ -> throwIO exception
{-# INLINE ignoringExceptions #-}
