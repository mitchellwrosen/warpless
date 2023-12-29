module Warpless.Settings
  ( Settings (..),
    defaultSettings,
    defaultOnExceptionResponse,
  )
where

import Data.Streaming.Network (HostPreference)
import Network.HTTP.Types qualified as H
import Network.Wai
import Warpless.Prelude
import Warpless.Types

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and the various \'set\' functions to modify individual fields. For example:
--
-- > setTimeout 20 defaultSettings
data Settings = Settings
  { -- | Port to listen on. Default value: 3000
    settingsPort :: !Port,
    -- | Interface to bind to. Default value: HostIPv4
    settingsHost :: !HostPreference,
    -- | What to do with exceptions thrown by either the application or server.
    -- Default: 'defaultOnException'
    settingsOnException :: !(SomeException -> IO ()),
    -- | Perform no parsing on the rawPathInfo.
    --
    -- This is useful for writing HTTP proxies.
    --
    -- Default: False
    settingsNoParsePath :: !Bool,
    -- | A HTTP/2 server push log function. Default: no action.
    settingsServerPushLogger :: !(Request -> ByteString -> Integer -> IO ()),
    -- | Determines the maxium buffer size when sending `Builder` responses
    -- (See `responseBuilder`).
    --
    -- When sending a builder response warp uses a 16 KiB buffer to write the
    -- builder to. When that buffer is too small to fit the builder warp will
    -- free it and create a new one that will fit the builder.
    --
    -- To protect against allocating too large a buffer warp will error if the
    -- builder requires more than this maximum.
    --
    -- Default: 1049_000_000 = 1 MiB.
    settingsMaxBuilderResponseBufferSize :: !Int
  }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsPort = 3000,
      settingsHost = "*4",
      settingsOnException = \_ -> pure (),
      settingsNoParsePath = False,
      settingsServerPushLogger = \_ _ _ -> pure (),
      settingsMaxBuilderResponseBufferSize = 1049000000
    }

-- | Sending 500 for internal server errors.
defaultOnExceptionResponse :: Response
defaultOnExceptionResponse =
  responseLBS
    H.internalServerError500
    [(H.hContentType, "text/plain; charset=utf-8")]
    "Something went wrong"
