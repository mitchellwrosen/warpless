module Warpless.Run
  ( run,
  )
where

import Control.Exception (MaskingState (..), mask_, onException)
import Control.Monad (forever)
import Data.ByteString qualified as ByteString
import Data.Streaming.Network (bindPortTCP)
import GHC.IO (unsafeUnmask)
import Ki qualified
import Network.Socket qualified as Network
import Network.Wai (Application)
import UnliftIO qualified
import Warpless.Connection (cleanupConnection, connRecv, socketConnection)
import Warpless.Date (GMTDate)
import Warpless.Date qualified as DateCache
import Warpless.Exception (ignoringExceptions)
import Warpless.HTTP1 (http1)
import Warpless.HTTP2 (http2)
import Warpless.Settings (Settings (settingsHost, settingsPort))

-- | Run an 'Application' with the given 'Settings'.
-- This opens a listen socket on the port defined in 'Settings' and
-- calls 'runSettingsSocket'.
run :: Settings -> Application -> IO ()
run settings app =
  UnliftIO.bracket (bindPortTCP (settingsPort settings) (settingsHost settings)) Network.close \serverSocket -> do
    dateCache <- DateCache.initialize
    Ki.scoped \scope -> do
      mask_ do
        forever do
          (clientSocket, addr) <- Network.accept serverSocket
          _ :: Ki.Thread () <-
            Ki.forkWith scope Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible} do
              handleClient settings app dateCache clientSocket addr
          pure ()
  where

handleClient :: Settings -> Application -> IO GMTDate -> Network.Socket -> Network.SockAddr -> IO ()
handleClient settings app getDate clientSocket addr = do
  -- NoDelay causes an error for AF_UNIX.
  ignoringExceptions (Network.setSocketOption clientSocket Network.NoDelay 1)
  conn <- socketConnection clientSocket
  let action = do
        -- fixme: Upgrading to HTTP/2 should be supported.
        bytes <- connRecv conn
        if ByteString.length bytes >= 4 && "PRI " `ByteString.isPrefixOf` bytes
          then http2 settings getDate conn app addr bytes
          else http1 settings getDate conn app addr bytes
  unsafeUnmask action `onException` cleanupConnection conn
  cleanupConnection conn
