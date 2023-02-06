module Warpless.File
  ( RspFileInfo (..),
    conditionalRequest,
    addContentHeadersForFilePart,
    H.parseByteRanges,
  )
where

import Control.Applicative ((<|>))
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8 (pack)
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Date
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as H
import Network.Wai
import Numeric (showInt)
import Warpless.FileInfoCache qualified as I
import Warpless.Header
import Warpless.PackInt

----------------------------------------------------------------

data RspFileInfo
  = WithoutBody !H.Status
  | WithBody !H.Status !H.ResponseHeaders !Integer !Integer
  deriving stock (Eq, Show)

----------------------------------------------------------------

conditionalRequest ::
  I.FileInfo ->
  H.ResponseHeaders ->
  -- | Response
  IndexedHeader ->
  -- | Request
  IndexedHeader ->
  RspFileInfo
conditionalRequest finfo hs0 rspidx reqidx = case condition of
  nobody@(WithoutBody _) -> nobody
  WithBody s _ off len ->
    let !hs1 = addContentHeaders hs0 off len size
        !hasLM = isJust $ rspidx ! fromEnum ResLastModified
        !hs = [(H.hLastModified, date) | not hasLM] ++ hs1
     in WithBody s hs off len
  where
    !mtime = I.fileInfoTime finfo
    !size = I.fileInfoSize finfo
    !date = I.fileInfoDate finfo
    !mcondition =
      ifmodified reqidx size mtime
        <|> ifunmodified reqidx size mtime
        <|> ifrange reqidx size mtime
    !condition = fromMaybe (unconditional reqidx size) mcondition

----------------------------------------------------------------

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince reqidx = reqidx ! fromEnum ReqIfModifiedSince >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince reqidx = reqidx ! fromEnum ReqIfUnmodifiedSince >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange reqidx = reqidx ! fromEnum ReqIfRange >>= parseHTTPDate

----------------------------------------------------------------

ifmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifmodified reqidx size mtime = do
  date <- ifModifiedSince reqidx
  return $
    if date /= mtime
      then unconditional reqidx size
      else WithoutBody H.notModified304

ifunmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifunmodified reqidx size mtime = do
  date <- ifUnmodifiedSince reqidx
  return $
    if date == mtime
      then unconditional reqidx size
      else WithoutBody H.preconditionFailed412

ifrange :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifrange reqidx size mtime = do
  date <- ifRange reqidx
  rng <- reqidx ! fromEnum ReqRange
  return $
    if date == mtime
      then parseRange rng size
      else WithBody H.ok200 [] 0 size

unconditional :: IndexedHeader -> Integer -> RspFileInfo
unconditional reqidx size = case reqidx ! fromEnum ReqRange of
  Nothing -> WithBody H.ok200 [] 0 size
  Just rng -> parseRange rng size

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
      C8.pack
      -- building with ShowS
      $
        'b' :
        'y' :
        't' :
        'e' :
        's' :
        ' ' :
        ( if beg > end
            then ('*' :)
            else
              showInt beg
                . ('-' :)
                . showInt end
        )
          ( '/' :
            showInt total ""
          )

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer -> H.ResponseHeaders
addContentHeaders hs off len size
  | len == size = hs'
  | otherwise =
      let !ctrng = contentRangeHeader off (off + len - 1) size
       in ctrng : hs'
  where
    !lengthBS = packIntegral len
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
