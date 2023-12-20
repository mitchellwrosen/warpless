module Warpless.CommonResponseHeaders
  ( CommonResponseHeaders,
    make,
    getContentLength,
    getDate,
    getLastModified,
  )
where

import Control.Monad.ST (ST)
import Data.Array (Array, (!))
import Data.Array.ST (MArray (newArray), STArray, runSTArray, writeArray)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive (foldedCase)
import GHC.Enum (Bounded, Enum, fromEnum, maxBound)
import Network.HTTP.Types (Header, HeaderName, ResponseHeaders)
import Warpless.Prelude
import Warpless.Types (HeaderValue)

-- | Common response headers.
newtype CommonResponseHeaders
  = CommonResponseHeaders (Array Int (Maybe HeaderValue))

make :: ResponseHeaders -> CommonResponseHeaders
make headers =
  CommonResponseHeaders (make_ headers)

make_ :: [Header] -> Array Int (Maybe HeaderValue)
make_ header =
  runSTArray do
    arr <- newArray (0, fromEnum (maxBound @ResponseHeaderIndex)) Nothing
    traverse_ (insert arr) header
    pure arr
  where
    insert :: STArray s Int (Maybe HeaderValue) -> Header -> ST s ()
    insert arr (key, val) =
      when (idx /= -1) do
        writeArray arr idx (Just val)
      where
        idx = responseKeyIndex key

data ResponseHeaderIndex
  = ResContentLength
  | ResServer
  | ResDate
  | ResLastModified
  deriving stock (Enum, Bounded)

responseKeyIndex :: HeaderName -> Int
responseKeyIndex (foldedCase -> name) =
  case ByteString.length name of
    4 | name == "date" -> fromEnum ResDate
    6 | name == "server" -> fromEnum ResServer
    13 | name == "last-modified" -> fromEnum ResLastModified
    14 | name == "content-length" -> fromEnum ResContentLength
    _ -> -1

getContentLength :: CommonResponseHeaders -> Maybe HeaderValue
getContentLength (CommonResponseHeaders headers) = do
  headers ! fromEnum ResContentLength

getDate :: CommonResponseHeaders -> Maybe HeaderValue
getDate (CommonResponseHeaders headers) = do
  headers ! fromEnum ResDate

getLastModified :: CommonResponseHeaders -> Maybe HeaderValue
getLastModified (CommonResponseHeaders headers) = do
  headers ! fromEnum ResLastModified
