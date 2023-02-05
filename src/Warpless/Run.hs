module Warpless.Run
  ( run,
    runSocket,
    runSettingsConnection,
    runSettingsConnectionMaker,
    runSettingsConnectionMakerSecure,
    socketConnection,
    setSocketCloseOnExec,
    withII,
  )
where

import Control.Arrow (first)
import Control.Exception (allowInterrupt)
import Control.Exception qualified
import Data.ByteString qualified as S
import Data.IORef (newIORef, readIORef)
import Data.Streaming.Network (bindPortTCP)
import Foreign.C.Error (Errno (..), eCONNABORTED, eMFILE)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket (SockAddr, Socket, SocketOption (..), close, fdSocket, gracefulClose, setSocketOption, withSocketsDo)
import Network.Socket.BufferPool
import Network.Socket.ByteString qualified as Sock
import Network.Wai
import System.IO.Error (ioeGetErrorType)
import System.TimeManager qualified as T
import System.Timeout (timeout)
import UnliftIO (toException)
import UnliftIO qualified
import Warpless.Buffer
import Warpless.Counter
import Warpless.Date qualified as D
import Warpless.FdCache qualified as F
import Warpless.FileInfoCache qualified as I
import Warpless.HTTP1 (http1)
import Warpless.HTTP2 (http2)
import Warpless.HTTP2.Types (isHTTP2)
import Warpless.Imports hiding (readInt)
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
        handler :: UnliftIO.IOException -> IO ByteString
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
  withSocketsDo $
    UnliftIO.bracket
      (bindPortTCP (settingsPort set) (settingsHost set))
      close
      ( \socket -> do
          setSocketCloseOnExec socket
          runSocket set socket app
      )

-- | This installs a shutdown handler for the given socket and
-- calls 'runSettingsConnection' with the default connection setup action
-- which handles plain (non-cipher) HTTP.
-- When the listen socket in the second argument is closed, all live
-- connections are gracefully shut down.
--
-- The supplied socket can be a Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSocket :: Settings -> Socket -> Application -> IO ()
runSocket set@Settings {settingsAccept = accept'} socket app = do
  settingsInstallShutdownHandler set closeListenSocket
  runSettingsConnection set getConn app
  where
    getConn = do
      (s, sa) <- accept' socket
      setSocketCloseOnExec s
      -- NoDelay causes an error for AF_UNIX.
      setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(UnliftIO.SomeException _) -> return ()
      conn <- socketConnection set s
      return (conn, sa)

    closeListenSocket = close socket

-- | The connection setup action would be expensive. A good example
-- is initialization of TLS.
-- So, this converts the connection setup action to the connection maker
-- which will be executed after forking a new worker thread.
-- Then this calls 'runSettingsConnectionMaker' with the connection maker.
-- This allows the expensive computations to be performed
-- in a separate worker thread instead of the main server loop.
--
-- Since 1.3.5
runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = runSettingsConnectionMaker set getConnMaker app
  where
    getConnMaker :: IO (IO Connection, SockAddr)
    getConnMaker = do
      (conn, sa) <- getConn
      return (return conn, sa)

-- | This modifies the connection maker so that it returns 'TCP' for 'Transport'
-- (i.e. plain HTTP) then calls 'runSettingsConnectionMakerSecure'.
runSettingsConnectionMaker :: Settings -> IO (IO Connection, SockAddr) -> Application -> IO ()
runSettingsConnectionMaker x y =
  runSettingsConnectionMakerSecure x (toTCP <$> y)
  where
    toTCP :: (IO Connection, SockAddr) -> (IO (Connection, Transport), SockAddr)
    toTCP = first ((,TCP) <$>)

----------------------------------------------------------------

-- | The core run function which takes 'Settings',
-- a connection maker and 'Application'.
-- The connection maker can return a connection of either plain HTTP
-- or HTTP over TLS.
--
-- Since 2.1.4
runSettingsConnectionMakerSecure :: Settings -> IO (IO (Connection, Transport), SockAddr) -> Application -> IO ()
runSettingsConnectionMakerSecure set getConnMaker app = do
  settingsBeforeMainLoop set
  counter <- newCounter
  withII set $ acceptConnection set getConnMaker app counter

-- | Running an action with internal info.
--
-- Since 3.3.11
withII :: Settings -> (InternalInfo -> IO a) -> IO a
withII set action =
  withTimeoutManager $ \tm ->
    D.withDateCache $ \dc ->
      F.withFdCache fdCacheDurationInSeconds $ \fdc ->
        I.withFileInfoCache fdFileInfoDurationInSeconds $ \fic -> do
          let ii = InternalInfo tm dc fdc fic
          action ii
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
  IO (IO (Connection, Transport), SockAddr) ->
  Application ->
  Counter ->
  InternalInfo ->
  IO ()
acceptConnection set getConnMaker app counter ii = do
  -- First mask all exceptions in acceptLoop. This is necessary to
  -- ensure that no async exception is throw between the call to
  -- acceptNewConnection and the registering of connClose.
  --
  -- acceptLoop can be broken by closing the listening socket.
  void $ UnliftIO.mask_ acceptLoop
  -- In some cases, we want to stop Warp here without graceful shutdown.
  -- So, async exceptions are allowed here.
  -- That's why `finally` is not used.
  gracefulShutdown set counter
  where
    acceptLoop = do
      -- Allow async exceptions before receiving the next connection maker.
      allowInterrupt

      -- acceptNewConnection will try to receive the next incoming
      -- request. It returns a /connection maker/, not a connection,
      -- since in some circumstances creating a working connection
      -- from a raw socket may be an expensive operation, and this
      -- expensive work should not be performed in the main event
      -- loop. An example of something expensive would be TLS
      -- negotiation.
      mx <- acceptNewConnection
      case mx of
        Nothing -> return ()
        Just (mkConn, addr) -> do
          fork set mkConn addr app counter ii
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
  IO (Connection, Transport) ->
  SockAddr ->
  Application ->
  Counter ->
  InternalInfo ->
  IO ()
fork set mkConn addr app counter ii = settingsFork set $ \unmask ->
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
    UnliftIO.bracket mkConn cleanUp (serve unmask)
  where
    cleanUp :: (Connection, Transport) -> IO ()
    cleanUp (conn, _) =
      connClose conn `UnliftIO.finally` do
        writeBuffer <- readIORef $ connWriteBuffer conn
        bufFree writeBuffer

    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit.
    serve :: (forall x. IO x -> IO x) -> (Connection, Transport) -> IO ()
    serve unmask (conn, transport) = UnliftIO.bracket register cancel $ \th -> do
      -- We now have fully registered a connection close handler in
      -- the case of all exceptions, so it is safe to once again
      -- allow async exceptions.
      unmask
        .
        -- Call the user-supplied code for connection open and
        -- close events
        UnliftIO.bracket (onOpen addr) (onClose addr)
        $ \goingon ->
          -- Actually serve this connection.  bracket with closeConn
          -- above ensures the connection is closed.
          when goingon $ serveConnection conn ii th addr transport set app
      where
        register = T.registerKillThread (timeoutManager ii) (connClose conn)
        cancel = T.cancel

    onOpen adr = increase counter >> settingsOnOpen set adr
    onClose :: SockAddr -> Bool -> IO ()
    onClose adr _ = decrease counter >> settingsOnClose set adr

serveConnection ::
  Connection ->
  InternalInfo ->
  T.Handle ->
  SockAddr ->
  Transport ->
  Settings ->
  Application ->
  IO ()
serveConnection conn ii th origAddr transport settings app = do
  -- fixme: Upgrading to HTTP/2 should be supported.
  (h2, bs) <-
    if isHTTP2 transport
      then return (True, "")
      else do
        bs0 <- connRecv conn
        if S.length bs0 >= 4 && "PRI " `S.isPrefixOf` bs0
          then return (True, bs0)
          else return (False, bs0)
  if settingsHTTP2Enabled settings && h2
    then do
      http2 settings ii conn transport app origAddr th bs
    else do
      http1 settings ii conn transport app origAddr th bs

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket = do
  fd <- fdSocket socket
  F.setFileCloseOnExec $ fromIntegral fd

gracefulShutdown :: Settings -> Counter -> IO ()
gracefulShutdown set counter =
  case settingsGracefulShutdownTimeout set of
    Nothing ->
      waitForZero counter
    (Just seconds) ->
      void (timeout (seconds * microsPerSecond) (waitForZero counter))
      where
        microsPerSecond = 1_000_000 :: Int
