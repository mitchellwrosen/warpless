module Warpless.Date
  ( withDateCache,
    GMTDate,
  )
where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction)
import Data.ByteString
import Network.HTTP.Date
import System.Posix (epochTime)

-- | The type of the Date header value.
type GMTDate = ByteString

-- | Creating 'DateCache' and executing the action.
withDateCache :: (IO GMTDate -> IO a) -> IO a
withDateCache action = initialize >>= action

initialize :: IO (IO GMTDate)
initialize =
  mkAutoUpdate
    defaultUpdateSettings
      { updateAction = formatHTTPDate <$> getCurrentHTTPDate
      }

getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate = epochTimeToHTTPDate <$> epochTime
