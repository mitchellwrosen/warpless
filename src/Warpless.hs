module Warpless
  ( run,

    -- * Settings
    Settings (..),
    defaultSettings,

    -- ** Exception response handler
    defaultOnExceptionResponse,

    -- * Data types
    HostPreference,
    Port,

    -- * Utilities
    FileInfo (..),
    getFileInfo,
  )
where

import Data.Streaming.Network (HostPreference)
import Warpless.FileInfo (FileInfo (..), getFileInfo)
import Warpless.Run (run)
import Warpless.Settings (Settings (..), defaultOnExceptionResponse, defaultSettings)
import Warpless.Types (Port)
