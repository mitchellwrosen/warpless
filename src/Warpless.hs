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

    -- * Temp testing
    helloWorld,
  )
where

import Data.Streaming.Network (HostPreference)
import Network.HTTP.Types
import Network.Socket qualified as Network
import Network.Wai (responseLBS)
import System.IO (print)
import Warpless.FileInfo (FileInfo (..), getFileInfo)
import Warpless.Prelude
import Warpless.Run (run)
import Warpless.Settings (Settings (..), defaultOnExceptionResponse, defaultSettings)
import Warpless.Types (Port)

helloWorld :: IO ()
helloWorld = do
  bracket (Network.socket Network.AF_INET Network.Stream Network.defaultProtocol) Network.close \socket -> do
    Network.bind socket (Network.SockAddrInet 8000 (Network.tupleToHostAddress (127, 0, 0, 1)))
    Network.listen socket Network.maxListenQueue
    run defaultSettings socket \request respond -> do
      print request
      respond (responseLBS status200 [] "Hello, world!")
