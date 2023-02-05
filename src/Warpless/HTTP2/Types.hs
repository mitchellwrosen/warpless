module Warpless.HTTP2.Types
  ( HTTP2Data (..),
    defaultHTTP2Data,
    PushPromise (..),
    defaultPushPromise,
  )
where

import Network.HTTP.Types qualified as H
import Network.HTTP2.Frame
import Network.HTTP2.Server qualified as H2
import Warpless.Imports

-- | HTTP/2 specific data.
--
--   Since: 3.2.7
data HTTP2Data = HTTP2Data
  { -- | Accessor for 'PushPromise' in 'HTTP2Data'.
    --
    --   Since: 3.2.7
    http2dataPushPromise :: ![PushPromise],
    -- | Accessor for 'H2.TrailersMaker' in 'HTTP2Data'.
    --
    --   Since: 3.2.8 but the type changed in 3.3.0
    http2dataTrailers :: !H2.TrailersMaker
  }

-- | Default HTTP/2 specific data.
--
--   Since: 3.2.7
defaultHTTP2Data :: HTTP2Data
defaultHTTP2Data = HTTP2Data [] H2.defaultTrailersMaker

-- | HTTP/2 push promise or sever push.
--   This allows files only for backward-compatibility
--   while the HTTP/2 library supports other types.
--
--   Since: 3.2.7
data PushPromise = PushPromise
  { -- | Accessor for a URL path in 'PushPromise'.
    --   E.g. \"\/style\/default.css\".
    --
    --   Since: 3.2.7
    promisedPath :: !ByteString,
    -- | Accessor for 'FilePath' in 'PushPromise'.
    --   E.g. \"FILE_PATH/default.css\".
    --
    --   Since: 3.2.7
    promisedFile :: !FilePath,
    -- | Accessor for 'H.ResponseHeaders' in 'PushPromise'
    --   \"content-type\" must be specified.
    --   Default value: [].
    --
    --
    --   Since: 3.2.7
    promisedResponseHeaders :: !H.ResponseHeaders,
    -- | Accessor for 'Weight' in 'PushPromise'.
    --    Default value: 16.
    --
    --   Since: 3.2.7
    promisedWeight :: !Weight
  }
  deriving stock (Eq, Ord, Show)

-- | Default push promise.
--
--   Since: 3.2.7
defaultPushPromise :: PushPromise
defaultPushPromise = PushPromise "" "" [] 16
