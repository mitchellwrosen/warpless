module Warpless.Date
  ( initialize,
    GMTDate,
  )
where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction)
import Data.ByteString
import Network.HTTP.Date
import System.Posix (epochTime)

-- | The type of the Date header value.
type GMTDate = ByteString

initialize :: IO (IO GMTDate)
initialize =
  mkAutoUpdate
    defaultUpdateSettings
      { updateAction = formatHTTPDate <$> getCurrentHTTPDate
      }

getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate = epochTimeToHTTPDate <$> epochTime
