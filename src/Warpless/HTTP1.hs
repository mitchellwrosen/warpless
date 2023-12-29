module Warpless.HTTP1
  ( http1,
  )
where

import Control.Concurrent qualified as Conc (yield)
import Data.ByteString qualified as ByteString
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import UnliftIO qualified
import Warpless.CommonRequestHeaders (CommonRequestHeaders)
import Warpless.CommonRequestHeaders qualified as CommonRequestHeaders
import Warpless.Connection (Connection)
import Warpless.Connection qualified as Connection
import Warpless.Date (GMTDate)
import Warpless.HTTP1.Request (receiveRequest)
import Warpless.HTTP1.Response (sendResponse)
import Warpless.HTTP1.Source (Source)
import Warpless.HTTP1.Source qualified as Source
import Warpless.Prelude
import Warpless.Settings (Settings (settingsOnException), defaultOnExceptionResponse)

http1 :: Settings -> IO GMTDate -> Connection -> Application -> SockAddr -> ByteString -> IO ()
http1 settings getDate conn application addr bytes0 = do
  -- Create a source of bytes from the client's connection.
  source <- Source.make (Connection.receive conn)

  -- We already read one chunk of bytes to determine if this was an HTTP2 request. It wasn't. Put those bytes back at
  -- the front of the source, for us to read next.
  Source.setLeftovers source bytes0

  let loop :: IO ()
      loop = do
        (request, commonHeaders, getBodyChunk) <- receiveRequest settings conn addr source
        keepAlive <- processRequest settings getDate conn application source request commonHeaders getBodyChunk
        when keepAlive loop
  loop

processRequest ::
  Settings ->
  IO GMTDate ->
  Connection ->
  Application ->
  Source ->
  Request ->
  CommonRequestHeaders ->
  IO ByteString ->
  IO Bool
processRequest settings getDate conn application source request commonRequestHeaders getBodyChunk = do
  keepAliveRef <- newIORef False

  result <-
    UnliftIO.tryAny do
      application request \response -> do
        keepAlive <- sendResponse conn getDate request commonRequestHeaders (Source.read source) response
        writeIORef keepAliveRef keepAlive
        pure ResponseReceived

  keepAlive <-
    case result of
      Right ResponseReceived -> readIORef keepAliveRef
      Left exception -> do
        settingsOnException settings exception
        _ <-
          sendResponse
            conn
            getDate
            request
            CommonRequestHeaders.empty
            (pure ByteString.empty)
            defaultOnExceptionResponse
        pure False

  -- We just send a Response and it takes a time to
  -- receive a Request again. If we immediately call recv,
  -- it is likely to fail and cause the IO manager to do some work.
  -- It is very costly, so we yield to another Haskell
  -- thread hoping that the next Request will arrive
  -- when this Haskell thread will be re-scheduled.
  -- This improves performance at least when
  -- the number of cores is small.
  Conc.yield

  -- If we are reusing this connection for another request, then first flush away all of the request body that (for
  -- whatever reason) was not read by the application).
  when keepAlive do
    let loop = do
          bytes <- getBodyChunk
          when (not (ByteString.null bytes)) loop
    loop

  pure keepAlive
