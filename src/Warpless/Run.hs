module Warpless.Run
  ( run,
  )
where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction, updateFreq)
import Control.Exception (MaskingState (..), allowInterrupt)
import Control.Monad (forever)
import Data.ByteString qualified as ByteString
import Data.Streaming.Network (bindPortTCP)
import GHC.IO (unsafeUnmask)
import Ki qualified
import Network.HTTP.Date (epochTimeToHTTPDate, formatHTTPDate)
import Network.Socket qualified as Network
import Network.Wai (Application)
import System.Posix (epochTime)
import Warpless.Connection qualified as Connection
import Warpless.Date (GMTDate)
import Warpless.Exception (ignoringExceptions)
import Warpless.HTTP1 (http1)
import Warpless.HTTP2 (http2)
import Warpless.Prelude
import Warpless.Settings (Settings, settingsHost, settingsPort)
import Warpless.Types (WeirdClient (WeirdClient))

run :: Settings -> Application -> IO ()
run settings app =
  -- Bind the server socket and run the application.
  --
  -- On exception, attempt to close the server socket.
  bracket
    (bindPortTCP (settingsPort settings) (settingsHost settings))
    (\socket -> uninterruptibleMask_ (ignoringExceptions (Network.close socket)))
    (run1 settings app)

run1 :: Settings -> Application -> Network.Socket -> IO b
run1 settings app serverSocket = do
  -- Create an IO action that gets the current date (formatted as a ByteString like "Wed, 20 Dec 2023 03:29:48 GMT").
  --
  -- The date is computed on-demand, at most once per second. So, any given date returned by this function may be up to
  -- one second in the past.
  getDate <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = formatHTTPDate . epochTimeToHTTPDate <$> epochTime,
          updateFreq = 1_000_000 -- microseconds
        }

  Ki.scoped \scope -> do
    -- Mask asynchronous exceptions, so we don't accept a client socket, then fail to close it due to an intervening
    -- asynchronous exception.
    mask_ do
      -- Accept clients in a loop, forever.
      --
      -- With asynchronous exceptions masked, we accept a client. We then fork a client handler on a thread with
      -- asynchronous exceptions uninterruptibly masked, and that thread installs an exception handler to close the client
      -- socket before it allows the delivery of asynchronous exceptions. Thus, every client socket we open is ultimately
      -- closed.
      forever do
        -- The main client `accept` is interruptible, so even with asynchronous exceptions (interruptibly) masked, if
        -- `accept` blocks, any pending asynchronous exception will be delivered.
        --
        -- However, just to be safe, we explicitly poll for asynchronous exceptions once per client accepted. This means
        -- that even in a hot accept-loop that never blocks, we'll still promptly receive asynchronous exceptions.
        allowInterrupt

        -- Accept a client!
        (clientSocket, clientAddr) <- Network.accept serverSocket

        -- Fork a thread (in this scope) to handle the client, with asynchronous exceptions uninterruptibly masked.
        -- The client thread is meant to arrange so that by the time asynchronous exceptions are allowed again, an
        -- exception handler is installed to close the client socket.
        --
        -- The one exception we allow to propagate all the way up here is the distinguished WeirdClient exception,
        -- which means the client did something wrong or weird, not us. We don't want that to bring down the server.
        -- So, we catch it here (so that it does not propagate to the main thread) and ignore it. Badly-written clients
        -- that may *think* they are speaking a dialogue of HTTP we can understand will just be ignored.
        void do
          Ki.forkWith @() scope clientThreadOptions do
            handleClient settings app getDate clientSocket clientAddr `catch` \WeirdClient -> pure ()
  where
    clientThreadOptions :: Ki.ThreadOptions
    clientThreadOptions =
      Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible}

handleClient :: Settings -> Application -> IO GMTDate -> Network.Socket -> Network.SockAddr -> IO ()
handleClient settings app getDate socket addr = do
  -- Disable Nagle's algorithm, which attempts to reduce network traffic by buffering writes until enough data is
  -- enqueued. Ignore exceptions, because on a unix socket, this operation throws an exception.
  ignoringExceptions (Network.setSocketOption socket Network.NoDelay 1)

  -- Wrap the socket in a connection object. This never throws an exception.
  conn <- Connection.create socket

  -- Guaranteeing that `Connection.cose conn` will be called, unmask asynchronous exceptions and handle the client!
  --
  -- N.B. we choose not to use the `finally` combinator only because we know asynchronous exceptions are already
  -- uninterruptibly masked, so we need not waste (hardly any) time masking them again.
  (`onException` Connection.close conn) do
    unsafeUnmask do
      -- fixme: Upgrading to HTTP/2 should be supported.
      bytes <- Connection.receive conn
      if ByteString.length bytes >= 4 && "PRI " `ByteString.isPrefixOf` bytes
        then http2 settings getDate conn app addr bytes
        else http1 settings getDate conn app addr bytes
  Connection.close conn
