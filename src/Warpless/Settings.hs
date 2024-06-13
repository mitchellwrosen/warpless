module Warpless.Settings
  ( Settings (..),
    defaultSettings,
    defaultOnExceptionResponse,
  )
where

import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Warpless.Prelude

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and the various \'set\' functions to modify individual fields. For example:
--
-- > setTimeout 20 defaultSettings
data Settings = Settings
  { -- | What to do with exceptions thrown by either the application or server.
    -- Default: 'defaultOnException'
    settingsOnException :: !(SomeException -> IO ())
  }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsOnException = \_ -> pure ()
    }

-- | Sending 500 for internal server errors.
defaultOnExceptionResponse :: Wai.Response
defaultOnExceptionResponse =
  Wai.responseLBS
    Http.internalServerError500
    [(Http.hContentType, "text/plain; charset=utf-8")]
    "Something went wrong"
