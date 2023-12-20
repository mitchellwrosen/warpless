-- | Exception utils.
module Warpless.Exception
  ( ignoringExceptions,
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
