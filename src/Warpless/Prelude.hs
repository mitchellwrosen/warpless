module Warpless.Prelude
  ( module Reexports,
    doWhile,
  )
where

import Control.Applicative as Reexports (pure)
import Control.Exception as Reexports
  ( Exception,
    IOException,
    SomeAsyncException,
    SomeException,
    assert,
    bracket,
    catch,
    fromException,
    mask_,
    onException,
    throwIO,
    try,
    uninterruptibleMask_,
  )
import Control.Monad as Reexports (guard, when, (=<<), (>>=))
import Data.Bool as Reexports (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as Reexports (ByteString)
import Data.Coerce as Reexports (coerce)
import Data.Either as Reexports (Either (Left, Right))
import Data.Eq as Reexports (Eq, (/=), (==))
import Data.Foldable as Reexports (for_, traverse_)
import Data.Function as Reexports (id, ($), (.))
import Data.Functor as Reexports (void, (<$>))
import Data.IORef as Reexports (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int as Reexports (Int, Int64)
import Data.Maybe as Reexports (Maybe (Just, Nothing), catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybe)
import Data.Ord as Reexports (Ord, Ordering (EQ, GT, LT), compare, max, min, (<), (<=), (>), (>=))
import Data.Semigroup as Reexports ((<>))
import Data.Traversable as Reexports (traverse)
import Data.Word as Reexports (Word, Word64, Word8)
import GHC.Base as Reexports (($!))
import GHC.Err as Reexports (error, undefined)
import GHC.Integer as Reexports (Integer)
import GHC.Num as Reexports ((*), (+), (-))
import GHC.Real as Reexports (divMod, mod)
import System.IO as Reexports (FilePath, IO)
import Text.Show as Reexports (Show, show)
import Witch as Reexports (from, unsafeFrom)

-- | Perform an action until it returns false.
doWhile :: IO Bool -> IO ()
doWhile action =
  loop
  where
    loop = do
      result <- action
      when result loop
