{-# OPTIONS_GHC -fno-warn-deprecations #-}

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
    runSocket,

    -- * Settings
    Settings (..),
    defaultSettings,
    ProxyProtocol (..),

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
    pauseTimeout,
    FileInfo (..),
    getFileInfo,
    withApplication,
    withApplicationSettings,
    testWithApplication,
    testWithApplicationSettings,
    openFreePort,

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
import Data.Vault.Lazy qualified as Vault
import Network.Wai (Request, vault)
import UnliftIO.Exception (throwIO)
import Warpless.FileInfoCache
import Warpless.HTTP2.Request (getHTTP2Data, modifyHTTP2Data, setHTTP2Data)
import Warpless.HTTP2.Types
import Warpless.Imports
import Warpless.Request
import Warpless.Response (warpVersion)
import Warpless.Run
import Warpless.Settings
import Warpless.Types hiding (getFileInfo)
import Warpless.WithApplication

-- | Explicitly pause the slowloris timeout.
--
-- This is useful for cases where you partially consume a request body. For
-- more information, see <https://github.com/yesodweb/wai/issues/351>
--
-- Since 3.0.10
pauseTimeout :: Request -> IO ()
pauseTimeout = fromMaybe (return ()) . Vault.lookup pauseTimeoutKey . vault

-- | Getting file information of the target file.
--
--   This function first uses a stat(2) or similar system call
--   to obtain information of the target file, then registers
--   it into the internal cache.
--   From the next time, the information is obtained
--   from the cache. This reduces the overhead to call the system call.
--   The internal cache is refreshed every duration specified by
--   'setFileInfoCacheDuration'.
--
--   This function throws an 'IO' exception if the information is not
--   available. For instance, the target file does not exist.
--   If this function is used an a Request generated by a WAI
--   backend besides Warp, it also throws an 'IO' exception.
--
-- Since 3.1.10
getFileInfo :: Request -> FilePath -> IO FileInfo
getFileInfo = fromMaybe (\_ -> throwIO (userError "getFileInfo")) . Vault.lookup getFileInfoKey . vault
