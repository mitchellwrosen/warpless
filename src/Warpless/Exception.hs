-- | Exception utils.
module Warpless.Exception
  ( ignoringExceptions,
    isSyncException,
  )
where

import Warpless.Prelude

ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catch` \(exception :: SomeException) ->
    case fromException @SomeAsyncException exception of
      Nothing -> pure ()
      Just _ -> throwIO exception
{-# INLINE ignoringExceptions #-}

isSyncException :: SomeException -> Bool
isSyncException =
  isNothing . fromException @SomeAsyncException
{-# INLINE isSyncException #-}
