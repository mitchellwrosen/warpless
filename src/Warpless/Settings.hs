{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Warpless.Settings where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.Streaming.Network (HostPreference)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import GHC.IO (IO (IO), unsafeUnmask)
import GHC.IO.Exception (AsyncException (ThreadKilled), IOErrorType (..))
import GHC.Prim (fork#)
import Network.HTTP.Types qualified as H
import Network.Socket (SockAddr, Socket, accept)
import Network.Wai
import Paths_warpless qualified
import System.IO (stderr)
import System.IO.Error (ioeGetErrorType)
import System.TimeManager
import UnliftIO (SomeException, fromException)
import Warpless.Imports
import Warpless.Types

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and the various \'set\' functions to modify individual fields. For example:
--
-- > setTimeout 20 defaultSettings
data Settings = Settings
  { -- | Port to listen on. Default value: 3000
    settingsPort :: !Port,
    -- | Default value: HostIPv4
    settingsHost :: !HostPreference,
    -- | What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    settingsOnException :: !(Maybe Request -> SomeException -> IO ()),
    -- | A function to create `Response` when an exception occurs.
    --
    -- Default: 500, text/plain, \"Something went wrong\"
    --
    -- Since 2.0.3
    settingsOnExceptionResponse :: !(SomeException -> Response),
    -- | What to do when a connection is open. When 'False' is returned, the connection is closed immediately. Otherwise, the connection is going on. Default: always returns 'True'.
    settingsOnOpen :: !(SockAddr -> IO Bool),
    -- | What to do when a connection is close. Default: do nothing.
    settingsOnClose :: !(SockAddr -> IO ()),
    -- | Timeout value in seconds. Default value: 30
    settingsTimeout :: !Int,
    -- | Use an existing timeout manager instead of spawning a new one. If used, 'settingsTimeout' is ignored. Default is 'Nothing'
    settingsManager :: !(Maybe Manager),
    -- | Cache duration time of file descriptors in seconds. 0 means that the cache mechanism is not used. Default value: 0
    settingsFdCacheDuration :: !Int,
    -- | Cache duration time of file information in seconds. 0 means that the cache mechanism is not used. Default value: 0
    settingsFileInfoCacheDuration :: !Int,
    -- | Code to run after the listening socket is ready but before entering
    -- the main event loop. Useful for signaling to tests that they can start
    -- running, or to drop permissions after binding to a restricted port.
    --
    -- Default: do nothing.
    --
    -- Since 1.3.6
    settingsBeforeMainLoop :: !(IO ()),
    -- | Code to fork a new thread to accept a connection.
    --
    -- This may be useful if you need OS bound threads, or if
    -- you wish to develop an alternative threading model.
    --
    -- Default: 'defaultFork'
    --
    -- Since 3.0.4
    settingsFork :: !(((forall a. IO a -> IO a) -> IO ()) -> IO ()),
    -- | Code to accept a new connection.
    --
    -- Useful if you need to provide connected sockets from something other
    -- than a standard accept call.
    --
    -- Default: 'defaultAccept'
    --
    -- Since 3.3.24
    settingsAccept :: !(Socket -> IO (Socket, SockAddr)),
    -- | Perform no parsing on the rawPathInfo.
    --
    -- This is useful for writing HTTP proxies.
    --
    -- Default: False
    --
    -- Since 2.0.3
    settingsNoParsePath :: !Bool,
    -- | An action to install a handler (e.g. Unix signal handler)
    -- to close a listen socket.
    -- The first argument is an action to close the listen socket.
    --
    -- Default: no action
    --
    -- Since 3.0.1
    settingsInstallShutdownHandler :: !(IO () -> IO ()),
    -- | Default server name if application does not set one.
    --
    -- Since 3.0.2
    settingsServerName :: !ByteString,
    -- | See @setMaximumBodyFlush@.
    --
    -- Since 3.0.3
    settingsMaximumBodyFlush :: !(Maybe Int),
    -- | Specify usage of the PROXY protocol.
    --
    -- Since 3.0.5
    settingsProxyProtocol :: !ProxyProtocol,
    -- | Size of bytes read to prevent Slowloris protection. Default value: 2048
    --
    -- Since 3.1.2
    settingsSlowlorisSize :: !Int,
    -- | Whether to enable HTTP2 ALPN/upgrades. Default: True
    --
    -- Since 3.1.7
    settingsHTTP2Enabled :: !Bool,
    -- | A log function. Default: no action.
    --
    -- Since 3.1.10
    settingsLogger :: !(Request -> H.Status -> Maybe Integer -> IO ()),
    -- | A HTTP/2 server push log function. Default: no action.
    --
    -- Since 3.2.7
    settingsServerPushLogger :: !(Request -> ByteString -> Integer -> IO ()),
    -- | An optional timeout to limit the time (in seconds) waiting for
    -- a graceful shutdown of the web server.
    --
    -- Since 3.2.8
    settingsGracefulShutdownTimeout :: !(Maybe Int),
    -- | A timeout to limit the time (in milliseconds) waiting for
    -- FIN for HTTP/1.x. 0 means uses immediate close.
    -- Default: 0.
    --
    -- Since 3.3.5
    settingsGracefulCloseTimeout1 :: !Int,
    -- | A timeout to limit the time (in milliseconds) waiting for
    -- FIN for HTTP/2. 0 means uses immediate close.
    -- Default: 2000.
    --
    -- Since 3.3.5
    settingsGracefulCloseTimeout2 :: !Int,
    -- | Determines the maximum header size that Warp will tolerate when using HTTP/1.x.
    --
    -- Since 3.3.8
    settingsMaxTotalHeaderLength :: !Int,
    -- | Specify the header value of Alternative Services (AltSvc:).
    --
    -- Default: Nothing
    --
    -- Since 3.3.11
    settingsAltSvc :: !(Maybe ByteString),
    -- | Determines the maxium buffer size when sending `Builder` responses
    -- (See `responseBuilder`).
    --
    -- When sending a builder response warp uses a 16 KiB buffer to write the
    -- builder to. When that buffer is too small to fit the builder warp will
    -- free it and create a new one that will fit the builder.
    --
    -- To protect against allocating too large a buffer warp will error if the
    -- builder requires more than this maximum.
    --
    -- Default: 1049_000_000 = 1 MiB.
    --
    -- Since 3.3.22
    settingsMaxBuilderResponseBufferSize :: !Int
  }

-- | Specify usage of the PROXY protocol.
data ProxyProtocol
  = -- | See @setProxyProtocolNone@.
    ProxyProtocolNone
  | -- | See @setProxyProtocolRequired@.
    ProxyProtocolRequired
  | -- | See @setProxyProtocolOptional@.
    ProxyProtocolOptional

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsPort = 3000,
      settingsHost = "*4",
      settingsOnException = defaultOnException,
      settingsOnExceptionResponse = defaultOnExceptionResponse,
      settingsOnOpen = const $ return True,
      settingsOnClose = const $ return (),
      settingsTimeout = 30,
      settingsManager = Nothing,
      settingsFdCacheDuration = 0,
      settingsFileInfoCacheDuration = 0,
      settingsBeforeMainLoop = return (),
      settingsFork = defaultFork,
      settingsAccept = defaultAccept,
      settingsNoParsePath = False,
      settingsInstallShutdownHandler = const $ return (),
      settingsServerName = C8.pack $ "Warp/" ++ showVersion Paths_warpless.version,
      settingsMaximumBodyFlush = Just 8192,
      settingsProxyProtocol = ProxyProtocolNone,
      settingsSlowlorisSize = 2048,
      settingsHTTP2Enabled = True,
      settingsLogger = \_ _ _ -> return (),
      settingsServerPushLogger = \_ _ _ -> return (),
      settingsGracefulShutdownTimeout = Nothing,
      settingsGracefulCloseTimeout1 = 0,
      settingsGracefulCloseTimeout2 = 2000,
      settingsMaxTotalHeaderLength = 50 * 1024,
      settingsAltSvc = Nothing,
      settingsMaxBuilderResponseBufferSize = 1049000000
    }

-- | Apply the logic provided by 'defaultOnException' to determine if an
-- exception should be shown or not. The goal is to hide exceptions which occur
-- under the normal course of the web server running.
--
-- Since 2.1.3
defaultShouldDisplayException :: SomeException -> Bool
defaultShouldDisplayException se
  | Just ThreadKilled <- fromException se = False
  | Just (_ :: InvalidRequest) <- fromException se = False
  | Just (ioeGetErrorType -> et) <- fromException se,
    et == ResourceVanished || et == InvalidArgument =
      False
  | Just TimeoutThread <- fromException se = False
  | otherwise = True

-- | Printing an exception to standard error
--   if `defaultShouldDisplayException` returns `True`.
--
-- Since: 3.1.0
defaultOnException :: Maybe Request -> SomeException -> IO ()
defaultOnException _ e =
  when (defaultShouldDisplayException e) $
    TIO.hPutStrLn stderr $ T.pack $ show e

-- | Sending 400 for bad requests.
--   Sending 500 for internal server errors.
-- Since: 3.1.0
--   Sending 413 for too large payload.
--   Sending 431 for too large headers.
-- Since 3.2.27
defaultOnExceptionResponse :: SomeException -> Response
defaultOnExceptionResponse e
  | Just PayloadTooLarge <-
      fromException e =
      responseLBS
        H.status413
        [(H.hContentType, "text/plain; charset=utf-8")]
        "Payload too large"
  | Just RequestHeaderFieldsTooLarge <-
      fromException e =
      responseLBS
        H.status431
        [(H.hContentType, "text/plain; charset=utf-8")]
        "Request header fields too large"
  | Just (_ :: InvalidRequest) <-
      fromException e =
      responseLBS
        H.badRequest400
        [(H.hContentType, "text/plain; charset=utf-8")]
        "Bad Request"
  | otherwise =
      responseLBS
        H.internalServerError500
        [(H.hContentType, "text/plain; charset=utf-8")]
        "Something went wrong"

-- | Exception handler for the debugging purpose.
--   500, text/plain, a showed exception.
--
-- Since: 2.0.3.2
exceptionResponseForDebug :: SomeException -> Response
exceptionResponseForDebug e =
  responseBuilder
    H.internalServerError500
    [(H.hContentType, "text/plain; charset=utf-8")]
    $ "Exception: " <> Builder.stringUtf8 (show e)

-- | Similar to @forkIOWithUnmask@, but does not set up the default exception handler.
--
-- Since Warp will always install its own exception handler in forked threads, this provides
-- a minor optimization.
--
-- For inspiration of this function, see @rawForkIO@ in the @async@ package.
--
-- @since 3.3.17
defaultFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
defaultFork io =
  IO $ \s0 ->
    case io unsafeUnmask of
      IO io' ->
        case (fork# io' s0) of
          (# s1, _tid #) ->
            (# s1, () #)

-- | Standard "accept" call for a listening socket.
--
-- @since 3.3.24
defaultAccept :: Socket -> IO (Socket, SockAddr)
defaultAccept =
  accept
