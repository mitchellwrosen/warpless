---------------------------------------------------------
--
-- Module        : Warpless
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A fast, light-weight HTTP server handler for WAI.
--
---------------------------------------------------------

-- | A fast, light-weight HTTP server handler for WAI.
--
-- HTTP\/1.0, HTTP\/1.1 and HTTP\/2 are supported. For HTTP\/2,
-- Warp supports direct and ALPN (in TLS) but not upgrade.
--
-- Note on slowloris timeouts: to prevent slowloris attacks, timeouts are used
-- at various points in request receiving and response sending. One interesting
-- corner case is partial request body consumption; in that case, Warp's
-- timeout handling is still in effect, and the timeout will not be triggered
-- again. Therefore, it is recommended that once you start consuming the
-- request body, you either:
--
-- * consume the entire body promptly
--
-- * call the 'pauseTimeout' function
--
-- For more information, see <https://github.com/yesodweb/wai/issues/351>.
module Warpless
  ( -- * Run a Warp server

    -- | All of these automatically serve the same 'Application' over HTTP\/1,
    -- HTTP\/1.1, and HTTP\/2.
    run,

    -- * Settings
    Settings (..),
    defaultSettings,

    -- ** Exception handler
    defaultOnException,
    defaultShouldDisplayException,

    -- ** Exception response handler
    defaultOnExceptionResponse,

    -- * Data types
    HostPreference,
    Port,
    InvalidRequest (..),

    -- * Utilities
    FileInfo (..),
    getFileInfo,

    -- * Version
    warpVersion,

    -- * HTTP/2

    -- ** HTTP2 data
    HTTP2Data,
    http2dataPushPromise,
    http2dataTrailers,
    defaultHTTP2Data,
    getHTTP2Data,
    setHTTP2Data,
    modifyHTTP2Data,

    -- ** Push promise
    PushPromise,
    promisedPath,
    promisedFile,
    promisedResponseHeaders,
    promisedWeight,
    defaultPushPromise,
  )
where

import Data.Streaming.Network (HostPreference)
import Warpless.FileInfo (FileInfo (..), getFileInfo)
import Warpless.HTTP2.Request (getHTTP2Data, modifyHTTP2Data, setHTTP2Data)
import Warpless.HTTP2.Types
  ( HTTP2Data (http2dataPushPromise, http2dataTrailers),
    PushPromise
      ( promisedFile,
        promisedPath,
        promisedResponseHeaders,
        promisedWeight
      ),
    defaultHTTP2Data,
    defaultPushPromise,
  )
import Warpless.Response (warpVersion)
import Warpless.Run (run)
import Warpless.Settings
  ( Settings (..),
    defaultOnException,
    defaultOnExceptionResponse,
    defaultSettings,
    defaultShouldDisplayException,
  )
import Warpless.Types (InvalidRequest (..), Port)
