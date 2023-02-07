module Warpless.Run
  ( run,
  )
where

import Control.Concurrent.STM
import Control.Exception (SomeException (..), allowInterrupt, throwIO)
import Control.Exception qualified
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.IORef (newIORef, readIORef)
import Data.Streaming.Network (bindPortTCP)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket (SockAddr, Socket, SocketOption (..), close, fdSocket, gracefulClose, setSocketOption)
import Network.Socket.BufferPool
import Network.Socket.ByteString qualified as Sock
import Network.Wai
import System.IO.Error (ioeGetErrorType)
import System.TimeManager qualified as T
import UnliftIO qualified
import Warpless.Buffer
import Warpless.Date qualified as D
import Warpless.FdCache qualified as F
import Warpless.FileInfoCache qualified as I
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
        connRecvBuf = receiveBuf s,
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
    counter <- newTVarIO 0
    withTimeoutManager \tm ->
      D.withDateCache \dc ->
        F.withFdCache fdCacheDurationInSeconds \fdc ->
          I.withFileInfoCache fdFileInfoDurationInSeconds \fic -> do
            let ii = InternalInfo tm dc fdc fic
            UnliftIO.mask_ $
              forever do
                allowInterrupt
                (s, addr) <- settingsAccept set socket
                setSocketCloseOnExec s
                -- NoDelay causes an error for AF_UNIX.
                setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(SomeException _) -> pure ()
                conn <- socketConnection set s
                fork set conn addr app counter ii
  where
    !fdCacheDurationInSeconds = settingsFdCacheDuration set * 1_000_000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration set * 1_000_000
    timeoutInSeconds = settingsTimeout set * 1_000_000
    withTimeoutManager :: forall r. (T.Manager -> IO r) -> IO r
    withTimeoutManager f =
      case settingsManager set of
        Just tm -> f tm
        Nothing -> UnliftIO.bracket (T.initialize timeoutInSeconds) T.stopManager f

-- Fork a new worker thread for this connection maker, and ask for a
-- function to unmask (i.e., allow async exceptions to be thrown).
fork ::
  Settings ->
  Connection ->
  SockAddr ->
  Application ->
  TVar Int ->
  InternalInfo ->
  IO ()
fork set conn addr app counter ii =
  settingsFork set \unmask ->
    -- Call the user-supplied on exception code if any
    -- exceptions are thrown.
    --
    -- Intentionally using Control.Exception.handle, since we want to
    -- catch all exceptions and avoid them from propagating, even
    -- async exceptions. See:
    -- https://github.com/yesodweb/wai/issues/850
    Control.Exception.handle (settingsOnException set Nothing) $
      -- Run the connection maker to get a new connection, and ensure
      -- that the connection is closed. If the mkConn call throws an
      -- exception, we will leak the connection. If the mkConn call is
      -- vulnerable to attacks (e.g., Slowloris), we do nothing to
      -- protect the server. It is therefore vital that mkConn is well
      -- vetted.
      --
      -- We grab the connection before registering timeouts since the
      -- timeouts will be useless during connection creation, due to the
      -- fact that async exceptions are still masked.
      serve unmask `UnliftIO.finally` cleanUp
  where
    cleanUp :: IO ()
    cleanUp =
      connClose conn `UnliftIO.finally` do
        writeBuffer <- readIORef (connWriteBuffer conn)
        bufFree writeBuffer

    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit.
    serve :: (forall x. IO x -> IO x) -> IO ()
    serve unmask =
      UnliftIO.bracket register T.cancel \th -> do
        -- We now have fully registered a connection close handler in
        -- the case of all exceptions, so it is safe to once again
        -- allow async exceptions.
        unmask $
          -- Call the user-supplied code for connection open and
          -- close events
          UnliftIO.bracket (onOpen addr) (onClose addr) \goingon ->
            -- Actually serve this connection.  bracket with closeConn
            -- above ensures the connection is closed.
            when goingon do
              -- fixme: Upgrading to HTTP/2 should be supported.
              bs <- connRecv conn
              if settingsHTTP2Enabled set && S.length bs >= 4 && "PRI " `S.isPrefixOf` bs
                then http2 set ii conn app addr th bs
                else http1 set ii conn app addr th bs
      where
        register = T.registerKillThread (timeoutManager ii) (connClose conn)

    onOpen adr = do
      atomically (modifyTVar' counter \n -> n + 1)
      settingsOnOpen set adr
    onClose :: SockAddr -> Bool -> IO ()
    onClose adr _ = do
      atomically (modifyTVar' counter \n -> n - 1)
      settingsOnClose set adr

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket = do
  fd <- fdSocket socket
  F.setFileCloseOnExec $ fromIntegral fd
