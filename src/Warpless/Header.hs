module Warpless.Header
  ( IndexedHeader,
    indexRequestHeader,
    RequestHeaderIndex (..),
    defaultIndexRequestHeader,
    indexResponseHeader,
    ResponseHeaderIndex (..),
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array (Array, array)
import Data.Array.ST (MArray (newArray), STArray, runSTArray, writeArray)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types (Header, HeaderName, RequestHeaders, ResponseHeaders)
import Warpless.Types (HeaderValue)

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
type IndexedHeader =
  Array Int (Maybe HeaderValue)

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader hdr =
  traverseHeader hdr requestMaxIndex requestKeyIndex

data RequestHeaderIndex
  = ReqContentLength
  | ReqTransferEncoding
  | ReqExpect
  | ReqConnection
  | ReqRange
  | ReqHost
  | ReqIfModifiedSince
  | ReqIfUnmodifiedSince
  | ReqIfRange
  | ReqReferer
  | ReqUserAgent
  | ReqIfMatch
  | ReqIfNoneMatch
  deriving stock (Enum, Bounded)

-- | The size for 'IndexedHeader' for HTTP Request.
--   From 0 to this corresponds to:
--
-- - \"Content-Length\"
-- - \"Transfer-Encoding\"
-- - \"Expect\"
-- - \"Connection\"
-- - \"Range\"
-- - \"Host\"
-- - \"If-Modified-Since\"
-- - \"If-Unmodified-Since\"
-- - \"If-Range\"
-- - \"Referer\"
-- - \"User-Agent\"
-- - \"If-Match\"
-- - \"If-None-Match\"
requestMaxIndex :: Int
requestMaxIndex =
  fromEnum (maxBound @RequestHeaderIndex)

requestKeyIndex :: HeaderName -> Int
requestKeyIndex (foldedCase -> name) =
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

defaultIndexRequestHeader :: IndexedHeader
defaultIndexRequestHeader =
  array (0, requestMaxIndex) [(i, Nothing) | i <- [0 .. requestMaxIndex]]

----------------------------------------------------------------

indexResponseHeader :: ResponseHeaders -> IndexedHeader
indexResponseHeader hdr =
  traverseHeader hdr responseMaxIndex responseKeyIndex

data ResponseHeaderIndex
  = ResContentLength
  | ResServer
  | ResDate
  | ResLastModified
  deriving stock (Enum, Bounded)

-- | The size for 'IndexedHeader' for HTTP Response.
responseMaxIndex :: Int
responseMaxIndex = fromEnum (maxBound @ResponseHeaderIndex)

responseKeyIndex :: HeaderName -> Int
responseKeyIndex (foldedCase -> name) =
  case ByteString.length name of
    4 | name == "date" -> fromEnum ResDate
    6 | name == "server" -> fromEnum ResServer
    13 | name == "last-modified" -> fromEnum ResLastModified
    14 | name == "content-length" -> fromEnum ResContentLength
    _ -> -1

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader
traverseHeader hdr maxidx getIndex =
  runSTArray do
    arr <- newArray (0, maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert :: STArray s Int (Maybe HeaderValue) -> Header -> ST s ()
    insert arr (key, val) =
      when (idx /= -1) do
        writeArray arr idx (Just val)
      where
        idx = getIndex key
