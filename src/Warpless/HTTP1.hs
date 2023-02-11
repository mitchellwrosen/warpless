module Warpless.HTTP1
  ( http1,
  )
where

import Control.Concurrent qualified as Conc (yield)
import Control.Exception (SomeException, fromException, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IP (toHostAddress, toHostAddress6)
import Network.Socket (SockAddr (SockAddrInet, SockAddrInet6))
import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import UnliftIO qualified
import Warpless.Connection (Connection (..))
import Warpless.Header
import Warpless.ReadInt
import Warpless.Request
import Warpless.Response
import Warpless.Settings
import Warpless.Types

http1 :: Settings -> InternalInfo -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http1 settings ii conn app origAddr bs0 = do
  istatus <- newIORef True
  src <- mkSource (wrappedRecv istatus)
  leftoverSource src bs0
  addr <- getProxyProtocolAddr src
  http1server settings ii conn app addr istatus src
  where
    wrappedRecv istatus = do
      bs <- connRecv conn
      when (not (BS.null bs)) (writeIORef istatus True)
      return bs

    getProxyProtocolAddr src =
      case settingsProxyProtocol settings of
        ProxyProtocolNone ->
          return origAddr
        ProxyProtocolRequired -> do
          seg <- readSource src
          parseProxyProtocolHeader src seg
        ProxyProtocolOptional -> do
          seg <- readSource src
          if BS.isPrefixOf "PROXY " seg
            then parseProxyProtocolHeader src seg
            else do
              leftoverSource src seg
              return origAddr

    parseProxyProtocolHeader src seg = do
      let (header, seg') = BS.break (== 0x0d) seg -- 0x0d == CR
          maybeAddr = case BS.split 0x20 header of -- 0x20 == space
            ["PROXY", "TCP4", clientAddr, _, clientPort, _] ->
              case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                [a] ->
                  Just
                    ( SockAddrInet
                        (readInt clientPort)
                        (toHostAddress a)
                    )
                _ -> Nothing
            ["PROXY", "TCP6", clientAddr, _, clientPort, _] ->
              case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                [a] ->
                  Just
                    ( SockAddrInet6
                        (readInt clientPort)
                        0
                        (toHostAddress6 a)
                        0
                    )
                _ -> Nothing
            ("PROXY" : "UNKNOWN" : _) ->
              Just origAddr
            _ ->
              Nothing
      case maybeAddr of
        Nothing -> throwIO (BadProxyHeader (decodeAscii header))
        Just a -> do
          leftoverSource src (BS.drop 2 seg') -- drop CRLF
          return a

    decodeAscii = map (chr . fromEnum) . BS.unpack

http1server ::
  Settings ->
  InternalInfo ->
  Connection ->
  Application ->
  SockAddr ->
  IORef Bool ->
  Source ->
  IO ()
http1server settings ii conn app addr istatus src =
  loop True `UnliftIO.catchAny` handler
  where
    handler e
      -- See comment below referencing
      -- https://github.com/yesodweb/wai/issues/618
      | Just NoKeepAliveRequest <- fromException e = return ()
      -- No valid request
      | Just (BadFirstLine _) <- fromException e = return ()
      | otherwise = do
          _ <- sendErrorResponse settings ii conn istatus defaultRequest {remoteHost = addr} e
          throwIO e

    loop firstRequest = do
      (req, mremainingRef, idxhdr, nextBodyFlush) <- recvRequest firstRequest settings conn ii addr src
      keepAlive <-
        processRequest settings ii conn app istatus src req mremainingRef idxhdr nextBodyFlush
          `UnliftIO.catchAny` \e -> do
            settingsOnException settings (Just req) e
            -- Don't throw the error again to prevent calling settingsOnException twice.
            return False

      -- When doing a keep-alive connection, the other side may just
      -- close the connection. We don't want to treat that as an
      -- exceptional situation, so we pass in False to http1 (which
      -- in turn passes in False to recvRequest), indicating that
      -- this is not the first request. If, when trying to read the
      -- request headers, no data is available, recvRequest will
      -- throw a NoKeepAliveRequest exception, which we catch here
      -- and ignore. See: https://github.com/yesodweb/wai/issues/618

      when keepAlive $ loop False

processRequest ::
  Settings ->
  InternalInfo ->
  Connection ->
  Application ->
  IORef Bool ->
  Source ->
  Request ->
  Maybe (IORef Int) ->
  IndexedHeader ->
  IO ByteString ->
  IO Bool
processRequest settings ii conn app istatus src req mremainingRef idxhdr nextBodyFlush = do
  -- In the event that some scarce resource was acquired during
  -- creating the request, we need to make sure that we don't get
  -- an async exception before calling the ResponseSource.
  keepAliveRef <- newIORef $ error "keepAliveRef not filled"
  r <-
    UnliftIO.tryAny $
      app req \res -> do
        -- FIXME consider forcing evaluation of the res here to
        -- send more meaningful error messages to the user.
        -- However, it may affect performance.
        writeIORef istatus False
        keepAlive <- sendResponse settings conn ii req idxhdr (readSource src) res
        writeIORef keepAliveRef keepAlive
        return ResponseReceived
  case r of
    Right ResponseReceived -> return ()
    Left (e :: SomeException)
      | Just (ExceptionInsideResponseBody e') <- fromException e -> throwIO e'
      | otherwise -> do
          keepAlive <- sendErrorResponse settings ii conn istatus req e
          settingsOnException settings (Just req) e
          writeIORef keepAliveRef keepAlive

  keepAlive <- readIORef keepAliveRef

  -- We just send a Response and it takes a time to
  -- receive a Request again. If we immediately call recv,
  -- it is likely to fail and cause the IO manager to do some work.
  -- It is very costly, so we yield to another Haskell
  -- thread hoping that the next Request will arrive
  -- when this Haskell thread will be re-scheduled.
  -- This improves performance at least when
  -- the number of cores is small.
  Conc.yield

  if keepAlive
    then -- If there is an unknown or large amount of data to still be read
    -- from the request body, simple drop this connection instead of
    -- reading it all in to satisfy a keep-alive request.
    case settingsMaximumBodyFlush settings of
      Just maxToRead | maxToRead > 0 -> do
        let tryKeepAlive = do
              -- flush the rest of the request body
              isComplete <- flushBody nextBodyFlush maxToRead
              pure isComplete
        case mremainingRef of
          Just ref -> do
            remaining <- readIORef ref
            if remaining <= maxToRead
              then tryKeepAlive
              else return False
          Nothing -> tryKeepAlive
      _ -> do
        flushEntireBody nextBodyFlush
        pure True
    else return False

sendErrorResponse :: Settings -> InternalInfo -> Connection -> IORef Bool -> Request -> SomeException -> IO Bool
sendErrorResponse settings ii conn istatus req e = do
  status <- readIORef istatus
  if shouldSendErrorResponse e && status
    then sendResponse settings conn ii req defaultIndexRequestHeader (return BS.empty) errorResponse
    else return False
  where
    shouldSendErrorResponse se
      | Just ConnectionClosedByPeer <- fromException se = False
      | otherwise = True
    errorResponse = settingsOnExceptionResponse settings e

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody src =
  loop
  where
    loop = do
      bs <- src
      when (not (BS.null bs)) loop

flushBody ::
  -- | get next chunk
  IO ByteString ->
  -- | maximum to flush
  Int ->
  -- | True == flushed the entire body, False == we didn't
  IO Bool
flushBody src = loop
  where
    loop toRead = do
      bs <- src
      let toRead' = toRead - BS.length bs
      case () of
        ()
          | BS.null bs -> return True
          | toRead' >= 0 -> loop toRead'
          | otherwise -> return False
