module Warpless.Run
  ( run,
  )
where

import Control.Exception (MaskingState (..), SomeException (..), allowInterrupt)
import Control.Monad (forever)
import Data.ByteString qualified as ByteString
import Data.IORef (readIORef)
import Data.Streaming.Network (bindPortTCP)
import GHC.IO (unsafeUnmask)
import Ki qualified
import Network.Socket (Socket, SocketOption (..), close, fdSocket, setSocketOption)
import Network.Wai
import UnliftIO qualified
import Warpless.Connection (Connection (..), socketConnection)
import Warpless.Date qualified as DateCache
import Warpless.FdCache qualified as FdCache
import Warpless.FileInfoCache qualified as FileInfoCache
import Warpless.HTTP1 (http1)
import Warpless.HTTP2 (http2)
import Warpless.Settings
import Warpless.Types

-- | Run an 'Application' with the given 'Settings'.
-- This opens a listen socket on the port defined in 'Settings' and
-- calls 'runSettingsSocket'.
run :: Settings -> Application -> IO ()
run settings app =
  UnliftIO.bracket (bindPortTCP (settingsPort settings) (settingsHost settings)) close \socket -> do
    setSocketCloseOnExec socket
    settingsBeforeMainLoop settings
    dateCache <- DateCache.initialize
    FdCache.withFdCache fdCacheDurationInSeconds \fdc ->
      FileInfoCache.withFileInfoCache fdFileInfoDurationInSeconds \fic -> do
        let ii = InternalInfo dateCache fdc fic
        UnliftIO.mask_ $
          Ki.scoped \scope -> do
            forever do
              allowInterrupt
              (s, addr) <- settingsAccept settings socket
              setSocketCloseOnExec s
              -- NoDelay causes an error for AF_UNIX.
              setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(SomeException _) -> pure ()
              conn <- socketConnection settings s
              _ :: Ki.Thread () <-
                Ki.forkWith scope Ki.defaultThreadOptions {Ki.maskingState = MaskedInterruptible} do
                  let cleanup = do
                        _ <- UnliftIO.tryAny (connClose conn)
                        writeBuffer <- readIORef (connWriteBuffer conn)
                        bufFree writeBuffer
                  (`UnliftIO.finally` cleanup) do
                    unsafeUnmask do
                      -- fixme: Upgrading to HTTP/2 should be supported.
                      bs <- connRecv conn
                      if settingsHTTP2Enabled settings && ByteString.length bs >= 4 && "PRI " `ByteString.isPrefixOf` bs
                        then http2 settings ii conn app addr bs
                        else http1 settings ii conn app addr bs
              pure ()
  where
    !fdCacheDurationInSeconds = settingsFdCacheDuration settings * 1_000_000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration settings * 1_000_000

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket = do
  fd <- fdSocket socket
  FdCache.setFileCloseOnExec $ fromIntegral fd
