module Warpless.HTTP2.Request
  ( toRequest,
    getHTTP2Data,
    setHTTP2Data,
    modifyHTTP2Data,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Vault.Lazy qualified as Vault
import Network.HPACK (TokenHeaderList, ValueTable, getHeaderValue)
import Network.HPACK.Token
import Network.HTTP.Types qualified as H
import Network.Socket (SockAddr)
import Network.Wai (RequestBodyLength (ChunkedBody, KnownLength))
import Network.Wai.Internal (Request (..))
import System.IO.Unsafe (unsafePerformIO)
import Warpless.HTTP2.Types (HTTP2Data)
import Warpless.Prelude
import Warpless.Settings qualified as S (Settings, settingsNoParsePath)

type ToReq = (TokenHeaderList, ValueTable) -> Maybe Int -> IO ByteString -> IO Request

----------------------------------------------------------------

toRequest :: S.Settings -> SockAddr -> ToReq
toRequest settings addr ht bodylen body = do
  ref <- newIORef Nothing
  toRequest' settings addr ref ht bodylen body

toRequest' ::
  S.Settings ->
  SockAddr ->
  IORef (Maybe HTTP2Data) ->
  ToReq
toRequest' settings addr ref (reqths, reqvt) bodylen body = pure req
  where
    !req =
      Request
        { requestMethod = colonMethod,
          httpVersion = H.http20,
          rawPathInfo = rawPath,
          pathInfo = H.decodePathSegments path,
          rawQueryString = query,
          queryString = H.parseQuery query,
          requestHeaders = headers,
          isSecure = False,
          remoteHost = addr,
          requestBody = body,
          vault = vaultValue,
          requestBodyLength = maybe ChunkedBody (KnownLength . unsafeFrom @Int @Word64) bodylen,
          requestHeaderHost = mHost <|> mAuth,
          requestHeaderRange = mRange,
          requestHeaderReferer = mReferer,
          requestHeaderUserAgent = mUserAgent
        }
    headers = List.map (first tokenKey) ths
      where
        ths = case mHost of
          Just _ -> reqths
          Nothing -> case mAuth of
            Just auth -> (tokenHost, auth) : reqths
            _ -> reqths
    !mPath = getHeaderValue tokenPath reqvt -- SHOULD
    !colonMethod = fromJust $ getHeaderValue tokenMethod reqvt -- MUST
    !mAuth = getHeaderValue tokenAuthority reqvt -- SHOULD
    !mHost = getHeaderValue tokenHost reqvt
    !mRange = getHeaderValue tokenRange reqvt
    !mReferer = getHeaderValue tokenReferer reqvt
    !mUserAgent = getHeaderValue tokenUserAgent reqvt
    -- CONNECT request will have ":path" omitted, use ":authority" as unparsed
    -- path instead so that it will have consistent behavior compare to HTTP 1.0
    (unparsedPath, query) = C8.break (== '?') $ fromJust (mPath <|> mAuth)
    !path = H.extractPath unparsedPath
    !rawPath = if S.settingsNoParsePath settings then unparsedPath else path
    -- fixme: pauseTimeout. th is not available here.
    !vaultValue =
      Vault.insert getHTTP2DataKey (readIORef ref)
        $ Vault.insert setHTTP2DataKey (writeIORef ref)
        $ Vault.insert modifyHTTP2DataKey (modifyIORef' ref)
        $ Vault.empty

getHTTP2DataKey :: Vault.Key (IO (Maybe HTTP2Data))
getHTTP2DataKey =
  unsafePerformIO Vault.newKey
{-# NOINLINE getHTTP2DataKey #-}

-- | Getting 'HTTP2Data' through vault of the request.
--   Warp uses this to receive 'HTTP2Data' from 'Middleware'.
--
--   Since: 3.2.7
getHTTP2Data :: Request -> IO (Maybe HTTP2Data)
getHTTP2Data req =
  case Vault.lookup getHTTP2DataKey (vault req) of
    Nothing -> pure Nothing
    Just getter -> getter

setHTTP2DataKey :: Vault.Key (Maybe HTTP2Data -> IO ())
setHTTP2DataKey = unsafePerformIO Vault.newKey
{-# NOINLINE setHTTP2DataKey #-}

-- | Setting 'HTTP2Data' through vault of the request.
--   'Application' or 'Middleware' should use this.
--
--   Since: 3.2.7
setHTTP2Data :: Request -> Maybe HTTP2Data -> IO ()
setHTTP2Data req mh2d =
  case Vault.lookup setHTTP2DataKey (vault req) of
    Nothing -> pure ()
    Just setter -> setter mh2d

modifyHTTP2DataKey :: Vault.Key ((Maybe HTTP2Data -> Maybe HTTP2Data) -> IO ())
modifyHTTP2DataKey = unsafePerformIO Vault.newKey
{-# NOINLINE modifyHTTP2DataKey #-}

-- | Modifying 'HTTP2Data' through vault of the request.
--   'Application' or 'Middleware' should use this.
--
--   Since: 3.2.8
modifyHTTP2Data :: Request -> (Maybe HTTP2Data -> Maybe HTTP2Data) -> IO ()
modifyHTTP2Data req func =
  case Vault.lookup modifyHTTP2DataKey (vault req) of
    Nothing -> pure ()
    Just modify -> modify func
