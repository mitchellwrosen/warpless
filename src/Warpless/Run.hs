module Warpless.Run
  ( run,
  )
where

import Control.Concurrent.STM
import Control.Exception (SomeException (..), allowInterrupt)
import Control.Exception qualified
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.Functor (void)
import Data.IORef (newIORef, readIORef)
import Data.Streaming.Network (bindPortTCP)
import Foreign.C.Error (Errno (..), eCONNABORTED, eMFILE)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket (SockAddr, Socket, SocketOption (..), close, fdSocket, gracefulClose, setSocketOption)
import Network.Socket.BufferPool
import Network.Socket.ByteString qualified as Sock
import Network.Wai
import System.IO.Error (ioeGetErrorType)
import System.TimeManager qualified as T
import System.Timeout (timeout)
import UnliftIO (toException)
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
          | otherwise = UnliftIO.throwIO e

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
    settingsInstallShutdownHandler set (close socket)
    let getConn :: IO (Connection, SockAddr)
        getConn = do
          (s, sa) <- settingsAccept set socket
          setSocketCloseOnExec s
          -- NoDelay causes an error for AF_UNIX.
          setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(SomeException _) -> pure ()
          conn <- socketConnection set s
          pure (conn, sa)
    settingsBeforeMainLoop set
    counter <- newTVarIO 0
    withII set $ acceptConnection set getConn app counter

-- | Running an action with internal info.
--
-- Since 3.3.11
withII :: Settings -> (InternalInfo -> IO a) -> IO a
withII set action =
  withTimeoutManager \tm ->
    D.withDateCache \dc ->
      F.withFdCache fdCacheDurationInSeconds \fdc ->
        I.withFileInfoCache fdFileInfoDurationInSeconds \fic ->
          action (InternalInfo tm dc fdc fic)
  where
    !fdCacheDurationInSeconds = settingsFdCacheDuration set * 1000000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration set * 1000000
    !timeoutInSeconds = settingsTimeout set * 1000000
    withTimeoutManager :: forall r. (T.Manager -> IO r) -> IO r
    withTimeoutManager f = case settingsManager set of
      Just tm -> f tm
      Nothing ->
        UnliftIO.bracket
          (T.initialize timeoutInSeconds)
          T.stopManager
          f

-- Note that there is a thorough discussion of the exception safety of the
-- following code at: https://github.com/yesodweb/wai/issues/146
--
-- We need to make sure of two things:
--
-- 1. Asynchronous exceptions are not blocked entirely in the main loop.
--    Doing so would make it impossible to kill the Warp thread.
--
-- 2. Once a connection maker is received via acceptNewConnection, the
--    connection is guaranteed to be closed, even in the presence of
--    async exceptions.
--
-- Our approach is explained in the comments below.
acceptConnection ::
  Settings ->
  IO (Connection, SockAddr) ->
  Application ->
  TVar Int ->
  InternalInfo ->
  IO ()
acceptConnection set getConnMaker app counter ii = do
  -- First mask all exceptions in acceptLoop. This is necessary to
  -- ensure that no async exception is throw between the call to
  -- acceptNewConnection and the registering of connClose.
  --
  -- acceptLoop can be broken by closing the listening socket.
  UnliftIO.mask_ acceptLoop
  -- In some cases, we want to stop Warp here without graceful shutdown.
  -- So, async exceptions are allowed here.
  -- That's why `finally` is not used.
  gracefulShutdown set counter
  where
    acceptLoop = do
      -- Allow async exceptions before receiving the next connection maker.
      allowInterrupt

      -- acceptNewConnection will try to receive the next incoming
      -- request.
      acceptNewConnection >>= \case
        Nothing -> pure ()
        Just (conn, addr) -> do
          fork set conn addr app counter ii
          acceptLoop

    acceptNewConnection = do
      ex <- UnliftIO.tryIO getConnMaker
      case ex of
        Right x -> return $ Just x
        Left e -> do
          let getErrno (Errno cInt) = cInt
              eConnAborted = getErrno eCONNABORTED
              eMfile = getErrno eMFILE
              merrno = ioe_errno e
          if merrno == Just eConnAborted || merrno == Just eMfile
            then acceptNewConnection
            else do
              settingsOnException set Nothing $ toException e
              return Nothing

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
            when goingon $ serveConnection conn ii th addr set app
      where
        register = T.registerKillThread (timeoutManager ii) (connClose conn)

    onOpen adr = do
      atomically (modifyTVar' counter \n -> n + 1)
      settingsOnOpen set adr
    onClose :: SockAddr -> Bool -> IO ()
    onClose adr _ = do
      atomically (modifyTVar' counter \n -> n - 1)
      settingsOnClose set adr

serveConnection ::
  Connection ->
  InternalInfo ->
  T.Handle ->
  SockAddr ->
  Settings ->
  Application ->
  IO ()
serveConnection conn ii th origAddr settings app = do
  -- fixme: Upgrading to HTTP/2 should be supported.
  bs <- connRecv conn
  if settingsHTTP2Enabled settings && S.length bs >= 4 && "PRI " `S.isPrefixOf` bs
    then http2 settings ii conn app origAddr th bs
    else http1 settings ii conn app origAddr th bs

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket = do
  fd <- fdSocket socket
  F.setFileCloseOnExec $ fromIntegral fd

gracefulShutdown :: Settings -> TVar Int -> IO ()
gracefulShutdown set counter =
  case settingsGracefulShutdownTimeout set of
    Nothing -> waitForZero
    Just seconds -> void (timeout (seconds * microsPerSecond) waitForZero)
  where
    microsPerSecond = 1_000_000 :: Int
    waitForZero =
      atomically do
        n <- readTVar counter
        when (n /= 0) retry
