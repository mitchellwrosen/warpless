module Warpless.Run
  ( run,
  )
where

import Control.Exception (MaskingState (..), SomeException (..), allowInterrupt, throwIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (newIORef, readIORef)
import Data.Streaming.Network (bindPortTCP)
import GHC.IO (unsafeUnmask)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Ki qualified
import Network.Socket (Socket, SocketOption (..), close, fdSocket, gracefulClose, setSocketOption)
import Network.Socket.BufferPool
import Network.Socket.ByteString qualified as Sock
import Network.Wai
import System.IO.Error (ioeGetErrorType)
import UnliftIO qualified
import Warpless.Buffer
import Warpless.Date qualified as DateCache
import Warpless.FdCache qualified as FdCache
import Warpless.FileInfoCache qualified as FileInfoCache
import Warpless.HTTP1 (http1)
import Warpless.HTTP2 (http2)
import Warpless.SendFile
import Warpless.Settings
import Warpless.Types

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Settings -> Socket -> IO Connection
socketConnection set s = do
  bufferPool <- newBufferPool 2048 16384
  writeBuffer <- createWriteBuffer 16384
  writeBufferRef <- newIORef writeBuffer
  isH2 <- newIORef False -- HTTP/1.x
  return
    Connection
      { connSendMany = Sock.sendMany s,
        connSendAll = sendall,
        connSendFile = sendfile writeBufferRef,
        connClose = do
          h2 <- readIORef isH2
          let tm =
                if h2
                  then settingsGracefulCloseTimeout2 set
                  else settingsGracefulCloseTimeout1 set
          if tm == 0
            then close s
            else gracefulClose s tm `UnliftIO.catchAny` \(UnliftIO.SomeException _) -> return (),
        connRecv = receive' s bufferPool,
        connRecvBuf = \_ _ -> pure True, -- obsoleted
        connWriteBuffer = writeBufferRef,
        connHTTP2 = isH2
      }
  where
    receive' sock pool = UnliftIO.handleIO handler $ receive sock pool
      where
        handler :: IOException -> IO ByteString
        handler e
          | ioeGetErrorType e == InvalidArgument = return ""
          | otherwise = throwIO e

    sendfile writeBufferRef fid offset len hook headers = do
      writeBuffer <- readIORef writeBufferRef
      sendFile
        s
        (bufBuffer writeBuffer)
        (bufSize writeBuffer)
        sendall
        fid
        offset
        len
        hook
        headers

    sendall = sendAll' s

    sendAll' sock bs =
      UnliftIO.handleJust
        ( \e ->
            if ioeGetErrorType e == ResourceVanished
              then Just ConnectionClosedByPeer
              else Nothing
        )
        UnliftIO.throwIO
        $ Sock.sendAll sock bs

-- | Run an 'Application' with the given 'Settings'.
-- This opens a listen socket on the port defined in 'Settings' and
-- calls 'runSettingsSocket'.
run :: Settings -> Application -> IO ()
run set app =
  UnliftIO.bracket (bindPortTCP (settingsPort set) (settingsHost set)) close \socket -> do
    setSocketCloseOnExec socket
    settingsBeforeMainLoop set
    dateCache <- DateCache.initialize
    FdCache.withFdCache fdCacheDurationInSeconds \fdc ->
      FileInfoCache.withFileInfoCache fdFileInfoDurationInSeconds \fic -> do
        let ii = InternalInfo dateCache fdc fic
        UnliftIO.mask_ $
          Ki.scoped \scope -> do
            forever do
              allowInterrupt
              (s, addr) <- settingsAccept set socket
              setSocketCloseOnExec s
              -- NoDelay causes an error for AF_UNIX.
              setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(SomeException _) -> pure ()
              conn <- socketConnection set s
              _ :: Ki.Thread () <-
                Ki.forkWith scope Ki.defaultThreadOptions {Ki.maskingState = MaskedInterruptible} do
                  -- th <- TimeManager.registerKillThread (timeoutManager ii) (connClose conn)
                  let cleanup = do
                        -- TimeManager.cancel th -- musn't throw
                        _ <- UnliftIO.tryAny (connClose conn)
                        writeBuffer <- readIORef (connWriteBuffer conn)
                        bufFree writeBuffer
                  (`UnliftIO.finally` cleanup) do
                    unsafeUnmask do
                      -- fixme: Upgrading to HTTP/2 should be supported.
                      bs <- connRecv conn
                      if settingsHTTP2Enabled set && ByteString.length bs >= 4 && "PRI " `ByteString.isPrefixOf` bs
                        then http2 set ii conn app addr bs
                        else http1 set ii conn app addr bs
              pure ()
  where
    !fdCacheDurationInSeconds = settingsFdCacheDuration set * 1_000_000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration set * 1_000_000

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket = do
  fd <- fdSocket socket
  FdCache.setFileCloseOnExec $ fromIntegral fd
