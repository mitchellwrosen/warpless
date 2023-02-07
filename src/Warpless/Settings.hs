{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Warpless.Settings
  ( Settings (..),
    defaultSettings,
    defaultShouldDisplayException,
    defaultOnException,
    defaultOnExceptionResponse,
    ProxyProtocol (..),
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Streaming.Network (HostPreference)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import GHC.IO.Exception (AsyncException (ThreadKilled), IOErrorType (..))
import Network.HTTP.Types qualified as H
import Network.Socket (SockAddr, Socket, accept)
import Network.Wai
import Paths_warpless qualified
import System.IO (stderr)
import System.IO.Error (ioeGetErrorType)
import System.TimeManager
import UnliftIO (SomeException, fromException)
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
    -- | Interface to bind to. Default value: HostIPv4
    settingsHost :: !HostPreference,
    -- | What to do with exceptions thrown by either the application or server.
    -- Default: 'defaultOnException'
    settingsOnException :: !(Maybe Request -> SomeException -> IO ()),
    -- | A function to create `Response` when an exception occurs.
    --
    -- Default: 500, text/plain, \"Something went wrong\"
    --
    -- Since 2.0.3
    settingsOnExceptionResponse :: !(SomeException -> Response),
    -- | "Slow-loris" timeout lower-bound value in seconds.  Connections where
    -- network progress is made less frequently than this may be closed.  In
    -- practice many connections may be allowed to go without progress for up to
    -- twice this amount of time.  Note that this timeout is not applied to
    -- application code, only network progress.
    --
    -- Default value: 30
    settingsTimeout :: !Int,
    -- | Cache duration time of file descriptors in seconds. 0 means that the cache mechanism is not used.
    --
    -- The FD cache is an optimization that is useful for servers dealing with
    -- static files. However, if files are being modified, it can cause incorrect
    -- results in some cases. Therefore, we disable it by default. If you know that
    -- your files will be static or you prefer performance to file consistency,
    -- it's recommended to turn this on; a reasonable value for those cases is 10.
    -- Enabling this cache results in drastic performance improvement for file
    -- transfers.
    --
    -- Default value: 0
    settingsFdCacheDuration :: !Int,
    -- | Cache duration time of file information in seconds. 0 means that the cache mechanism is not used.
    --
    -- The file information cache is an optimization that is useful for servers dealing with
    -- static files. However, if files are being modified, it can cause incorrect
    -- results in some cases. Therefore, we disable it by default. If you know that
    -- your files will be static or you prefer performance to file consistency,
    -- it's recommended to turn this on; a reasonable value for those cases is 10.
    -- Enabling this cache results in drastic performance improvement for file
    -- transfers.
    --
    -- Default value: 0
    settingsFileInfoCacheDuration :: !Int,
    -- | Code to run after the listening socket is ready but before entering
    -- the main event loop. Useful for signaling to tests that they can start
    -- running, or to drop permissions after binding to a restricted port.
    --
    -- Default: do nothing.
    settingsBeforeMainLoop :: !(IO ()),
    -- | Code to accept a new connection.
    --
    -- Useful if you need to provide connected sockets from something other
    -- than a standard accept call.
    --
    -- Default: 'defaultAccept'
    settingsAccept :: !(Socket -> IO (Socket, SockAddr)),
    -- | Perform no parsing on the rawPathInfo.
    --
    -- This is useful for writing HTTP proxies.
    --
    -- Default: False
    settingsNoParsePath :: !Bool,
    -- | Default server name to be sent as the \"Server:\" header
    --   if an application does not set one.
    --   If an empty string is set, the \"Server:\" header is not sent.
    --   This is true even if an application set one.
    settingsServerName :: !ByteString,
    -- | The maximum number of bytes to flush from an unconsumed request body.
    --
    -- By default, Warp does not flush the request body so that, if a large body is
    -- present, the connection is simply terminated instead of wasting time and
    -- bandwidth on transmitting it. However, some clients do not deal with that
    -- situation well. You can either change this setting to @Nothing@ to flush the
    -- entire body in all cases, or in your application ensure that you always
    -- consume the entire request body.
    --
    -- Default: 8192 bytes.
    settingsMaximumBodyFlush :: !(Maybe Int),
    -- | Specify usage of the PROXY protocol.
    settingsProxyProtocol :: !ProxyProtocol,
    -- | Size in bytes read to prevent Slowloris attacks. Default value: 2048
    settingsSlowlorisSize :: !Int,
    -- | Whether to enable HTTP2 ALPN/upgrades. Default: True
    settingsHTTP2Enabled :: !Bool,
    -- | A log function. Default: no action.
    settingsLogger :: !(Request -> H.Status -> Maybe Integer -> IO ()),
    -- | A HTTP/2 server push log function. Default: no action.
    settingsServerPushLogger :: !(Request -> ByteString -> Integer -> IO ()),
    -- | A timeout to limit the time (in milliseconds) waiting for
    -- FIN for HTTP/1.x. 0 means uses immediate close.
    -- Default: 0.
    settingsGracefulCloseTimeout1 :: !Int,
    -- | A timeout to limit the time (in milliseconds) waiting for
    -- FIN for HTTP/2. 0 means uses immediate close.
    -- Default: 2000.
    settingsGracefulCloseTimeout2 :: !Int,
    -- | Determines the maximum header size that Warp will tolerate when using HTTP/1.x.
    settingsMaxTotalHeaderLength :: !Int,
    -- | Specify the header value of Alternative Services (AltSvc:).
    --
    -- Default: Nothing
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
    settingsMaxBuilderResponseBufferSize :: !Int
  }

-- | Specify usage of the PROXY protocol.
data ProxyProtocol
  = -- | Do not use the PROXY protocol.
    ProxyProtocolNone
  | -- | Require PROXY header.
    --
    -- This is for cases where a "dumb" TCP/SSL proxy is being used, which cannot
    -- add an @X-Forwarded-For@ HTTP header field but has enabled support for the
    -- PROXY protocol.
    --
    -- See <http://www.haproxy.org/download/1.5/doc/proxy-protocol.txt> and
    -- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#proxy-protocol>.
    --
    -- Only the human-readable header format (version 1) is supported. The binary
    -- header format (version 2) is /not/ supported.
    ProxyProtocolRequired
  | -- | Use the PROXY header if it exists, but also accept
    -- connections without the header.  See 'setProxyProtocolRequired'.
    --
    -- WARNING: This is contrary to the PROXY protocol specification and
    -- using it can indicate a security problem with your
    -- architecture if the web server is directly accessible
    -- to the public, since it would allow easy IP address
    -- spoofing.  However, it can be useful in some cases,
    -- such as if a load balancer health check uses regular
    -- HTTP without the PROXY header, but proxied
    -- connections /do/ include the PROXY header.
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
      settingsTimeout = 30,
      settingsFdCacheDuration = 0,
      settingsFileInfoCacheDuration = 0,
      settingsBeforeMainLoop = return (),
      settingsAccept = defaultAccept,
      settingsNoParsePath = False,
      settingsServerName = C8.pack $ "Warp/" ++ showVersion Paths_warpless.version,
      settingsMaximumBodyFlush = Just 8192,
      settingsProxyProtocol = ProxyProtocolNone,
      settingsSlowlorisSize = 2048,
      settingsHTTP2Enabled = True,
      settingsLogger = \_ _ _ -> return (),
      settingsServerPushLogger = \_ _ _ -> return (),
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

-- | Standard "accept" call for a listening socket.
--
-- @since 3.3.24
defaultAccept :: Socket -> IO (Socket, SockAddr)
defaultAccept =
  accept
