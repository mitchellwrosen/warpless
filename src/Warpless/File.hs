module Warpless.File
  ( RspFileInfo (..),
    conditionalRequest,
    addContentHeadersForFilePart,
    H.parseByteRanges,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as ByteString.Builder
import Data.ByteString.Char8 qualified as C8 (pack)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Date (HTTPDate, parseHTTPDate)
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as H
import Network.Wai (FilePart (filePartByteCount, filePartFileSize, filePartOffset))
import Numeric (showInt)
import Warpless.FileInfo (FileInfo (..))
import Warpless.Header (IndexedHeader, RequestHeaderIndex (..), ResponseHeaderIndex (ResLastModified))

----------------------------------------------------------------

data RspFileInfo
  = WithoutBody !H.Status
  | WithBody !H.Status !H.ResponseHeaders !Integer !Integer
  deriving stock (Eq, Show)

----------------------------------------------------------------

conditionalRequest ::
  FileInfo ->
  H.ResponseHeaders ->
  H.Method ->
  -- | Response
  IndexedHeader ->
  -- | Request
  IndexedHeader ->
  RspFileInfo
conditionalRequest finfo hs0 method rspidx reqidx = case condition of
  nobody@(WithoutBody _) -> nobody
  WithBody s _ off len ->
    let !hs1 = addContentHeaders hs0 off len size
        !hs = case rspidx ! fromEnum ResLastModified of
          Just _ -> hs1
          Nothing -> (H.hLastModified, date) : hs1
     in WithBody s hs off len
  where
    !mtime = fileInfoTime finfo
    !size = fileInfoSize finfo
    !date = fileInfoDate finfo
    -- According to RFC 9110:
    -- "A recipient cache or origin server MUST evaluate the request
    -- preconditions defined by this specification in the following order:
    -- - If-Match
    -- - If-Unmodified-Since
    -- - If-None-Match
    -- - If-Modified-Since
    -- - If-Range
    --
    -- We don't actually implement the If-(None-)Match logic, but
    -- we also don't want to block middleware or applications from
    -- using ETags. And sending If-(None-)Match headers in a request
    -- to a server that doesn't use them is requester's problem.
    !mcondition =
      ifunmodified reqidx mtime
        <|> ifmodified reqidx mtime method
        <|> ifrange reqidx mtime method size

    !condition = fromMaybe (unconditional reqidx size) mcondition

----------------------------------------------------------------

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince reqidx = reqidx ! fromEnum ReqIfModifiedSince >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince reqidx = reqidx ! fromEnum ReqIfUnmodifiedSince >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange reqidx = reqidx ! fromEnum ReqIfRange >>= parseHTTPDate

----------------------------------------------------------------

ifmodified :: IndexedHeader -> HTTPDate -> H.Method -> Maybe RspFileInfo
ifmodified reqidx mtime method = do
  date <- ifModifiedSince reqidx
  -- According to RFC 9110:
  -- "A recipient MUST ignore If-Modified-Since if the request
  -- contains an If-None-Match header field; [...]"
  guard . isNothing $ reqidx ! fromEnum ReqIfNoneMatch
  -- "A recipient MUST ignore the If-Modified-Since header field
  -- if [...] the request method is neither GET nor HEAD."
  guard $ method == H.methodGet || method == H.methodHead
  guard $ date == mtime || date > mtime
  Just $ WithoutBody H.notModified304

ifunmodified :: IndexedHeader -> HTTPDate -> Maybe RspFileInfo
ifunmodified reqidx mtime = do
  date <- ifUnmodifiedSince reqidx
  -- According to RFC 9110:
  -- "A recipient MUST ignore If-Unmodified-Since if the request
  -- contains an If-Match header field; [...]"
  guard . isNothing $ reqidx ! fromEnum ReqIfMatch
  guard $ date /= mtime && date < mtime
  Just $ WithoutBody H.preconditionFailed412

ifrange :: IndexedHeader -> HTTPDate -> H.Method -> Integer -> Maybe RspFileInfo
ifrange reqidx mtime method size = do
  -- According to RFC 9110:
  -- "When the method is GET and both Range and If-Range are
  -- present, evaluate the If-Range precondition:"
  date <- ifRange reqidx
  rng <- reqidx ! fromEnum ReqRange
  guard $ method == H.methodGet
  pure
    if date == mtime
      then parseRange rng size
      else WithBody H.ok200 [] 0 size

unconditional :: IndexedHeader -> Integer -> RspFileInfo
unconditional reqidx =
  case reqidx ! fromEnum ReqRange of
    Nothing -> WithBody H.ok200 [] 0
    Just rng -> parseRange rng

----------------------------------------------------------------

parseRange :: ByteString -> Integer -> RspFileInfo
parseRange rng size = case H.parseByteRanges rng of
  Nothing -> WithoutBody H.requestedRangeNotSatisfiable416
  Just [] -> WithoutBody H.requestedRangeNotSatisfiable416
  Just (r : _) ->
    let (!beg, !end) = checkRange r size
        !len = end - beg + 1
        s =
          if beg == 0 && end == size - 1
            then H.ok200
            else H.partialContent206
     in WithBody s [] beg len

checkRange :: H.ByteRange -> Integer -> (Integer, Integer)
checkRange (H.ByteRangeFrom beg) size = (beg, size - 1)
checkRange (H.ByteRangeFromTo beg end) size = (beg, min (size - 1) end)
checkRange (H.ByteRangeSuffix count) size = (max 0 (size - count), size - 1)

----------------------------------------------------------------

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (H.hContentRange, range)
  where
    range =
      -- building with ShowS
      C8.pack $
        'b'
          : 'y'
          : 't'
          : 'e'
          : 's'
          : ' '
          : ( if beg > end
                then ('*' :)
                else showInt beg . ('-' :) . showInt end
            )
            ('/' : showInt total "")

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer -> H.ResponseHeaders
addContentHeaders hs off len size
  | len == size = hs'
  | otherwise =
      let !ctrng = contentRangeHeader off (off + len - 1) size
       in ctrng : hs'
  where
    !lengthBS = ByteString.Lazy.toStrict (ByteString.Builder.toLazyByteString (ByteString.Builder.integerDec len))
    !hs' = (H.hContentLength, lengthBS) : (H.hAcceptRanges, "bytes") : hs

-- |
--
-- >>> addContentHeadersForFilePart [] (FilePart 2 10 16)
-- [("Content-Range","bytes 2-11/16"),("Content-Length","10"),("Accept-Ranges","bytes")]
-- >>> addContentHeadersForFilePart [] (FilePart 0 16 16)
-- [("Content-Length","16"),("Accept-Ranges","bytes")]
addContentHeadersForFilePart :: H.ResponseHeaders -> FilePart -> H.ResponseHeaders
addContentHeadersForFilePart hs part = addContentHeaders hs off len size
  where
    off = filePartOffset part
    len = filePartByteCount part
    size = filePartFileSize part
