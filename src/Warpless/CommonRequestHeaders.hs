module Warpless.CommonRequestHeaders
  ( CommonRequestHeaders,
    empty,
    make,
    getConnection,
    getContentLength,
    getExpect,
    getHost,
    getIfMatch,
    getIfModifiedSince,
    getIfNoneMatch,
    getIfRange,
    getIfUnmodifiedSince,
    getRange,
    getReferer,
    getTransferEncoding,
    getUserAgent,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array (Array, array, (!))
import Data.Array.ST (MArray (newArray), STArray, runSTArray, writeArray)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Date (HTTPDate, parseHTTPDate)
import Network.HTTP.Types (Header, HeaderName, RequestHeaders)
import Warpless.Types (HeaderValue)

-- | Common request headers.
newtype CommonRequestHeaders
  = CommonRequestHeaders (Array Int (Maybe HeaderValue))

empty :: CommonRequestHeaders
empty =
  CommonRequestHeaders (array (0, maxIndex) [(i, Nothing) | i <- [0 .. maxIndex]])

make :: RequestHeaders -> CommonRequestHeaders
make headers =
  CommonRequestHeaders (make_ headers)

make_ :: [Header] -> Array Int (Maybe HeaderValue)
make_ header =
  runSTArray do
    arr <- newArray (0, maxIndex) Nothing
    mapM_ (insert arr) header
    pure arr
  where
    insert :: STArray s Int (Maybe HeaderValue) -> Header -> ST s ()
    insert arr (key, val) =
      when (idx /= -1) do
        writeArray arr idx (Just val)
      where
        idx = keyIndex key

keyIndex :: HeaderName -> Int
keyIndex (foldedCase -> name) =
  case ByteString.length name of
    4 | name == "host" -> fromEnum ReqHost
    5 | name == "range" -> fromEnum ReqRange
    6 | name == "expect" -> fromEnum ReqExpect
    7 | name == "referer" -> fromEnum ReqReferer
    8
      | name == "if-range" -> fromEnum ReqIfRange
      | name == "if-match" -> fromEnum ReqIfMatch
    10
      | name == "user-agent" -> fromEnum ReqUserAgent
      | name == "connection" -> fromEnum ReqConnection
    13 | name == "if-none-match" -> fromEnum ReqIfNoneMatch
    14 | name == "content-length" -> fromEnum ReqContentLength
    17
      | name == "transfer-encoding" -> fromEnum ReqTransferEncoding
      | name == "if-modified-since" -> fromEnum ReqIfModifiedSince
    19 | name == "if-unmodified-since" -> fromEnum ReqIfUnmodifiedSince
    _ -> -1

data RequestHeaderIndex
  = ReqConnection
  | ReqContentLength
  | ReqExpect
  | ReqHost
  | ReqIfMatch
  | ReqIfModifiedSince
  | ReqIfNoneMatch
  | ReqIfRange
  | ReqIfUnmodifiedSince
  | ReqRange
  | ReqReferer
  | ReqTransferEncoding
  | ReqUserAgent
  deriving stock (Bounded, Enum)

maxIndex :: Int
maxIndex =
  fromEnum (maxBound @RequestHeaderIndex)

getConnection :: CommonRequestHeaders -> Maybe HeaderValue
getConnection (CommonRequestHeaders headers) =
  headers ! fromEnum ReqConnection

getContentLength :: CommonRequestHeaders -> Maybe HeaderValue
getContentLength (CommonRequestHeaders headers) =
  headers ! fromEnum ReqContentLength

getExpect :: CommonRequestHeaders -> Maybe HeaderValue
getExpect (CommonRequestHeaders headers) =
  headers ! fromEnum ReqExpect

getHost :: CommonRequestHeaders -> Maybe HeaderValue
getHost (CommonRequestHeaders headers) =
  headers ! fromEnum ReqHost

getIfMatch :: CommonRequestHeaders -> Maybe HeaderValue
getIfMatch (CommonRequestHeaders headers) =
  headers ! fromEnum ReqIfMatch

getIfModifiedSince :: CommonRequestHeaders -> Maybe HTTPDate
getIfModifiedSince (CommonRequestHeaders headers) = do
  bytes <- headers ! fromEnum ReqIfModifiedSince
  parseHTTPDate bytes

getIfNoneMatch :: CommonRequestHeaders -> Maybe HeaderValue
getIfNoneMatch (CommonRequestHeaders headers) =
  headers ! fromEnum ReqIfNoneMatch

getIfRange :: CommonRequestHeaders -> Maybe HTTPDate
getIfRange (CommonRequestHeaders headers) = do
  bytes <- headers ! fromEnum ReqIfRange
  parseHTTPDate bytes

getIfUnmodifiedSince :: CommonRequestHeaders -> Maybe HTTPDate
getIfUnmodifiedSince (CommonRequestHeaders headers) = do
  bytes <- headers ! fromEnum ReqIfUnmodifiedSince
  parseHTTPDate bytes

getRange :: CommonRequestHeaders -> Maybe HeaderValue
getRange (CommonRequestHeaders headers) = do
  headers ! fromEnum ReqRange

getReferer :: CommonRequestHeaders -> Maybe HeaderValue
getReferer (CommonRequestHeaders headers) = do
  headers ! fromEnum ReqReferer

getTransferEncoding :: CommonRequestHeaders -> Maybe HeaderValue
getTransferEncoding (CommonRequestHeaders headers) = do
  headers ! fromEnum ReqTransferEncoding

getUserAgent :: CommonRequestHeaders -> Maybe HeaderValue
getUserAgent (CommonRequestHeaders headers) = do
  headers ! fromEnum ReqUserAgent
