module Warpless.RequestHeader
  ( parseHeaderLines,
  )
where

import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (ByteString (..), memchr)
import Data.CaseInsensitive qualified as CI
import Data.List qualified as List
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import GHC.Real (fromIntegral)
import Network.HTTP.Types qualified as Http
import Warpless.Byte qualified as Byte
import Warpless.Prelude
import Warpless.Types (WeirdClient (..))

parseHeaderLines ::
  [ByteString] ->
  IO
    ( Http.Method,
      ByteString, --  Unparsed path
      ByteString, --  Query
      Http.HttpVersion,
      Http.RequestHeaders
    )
parseHeaderLines = \case
  [] -> throwIO WeirdClient
  firstLine : otherLines -> do
    (method, path', query, httpversion) <- parseRequestLine firstLine
    pure (method, path', query, httpversion, List.map parseHeader otherLines)

----------------------------------------------------------------

-- >>> parseRequestLine "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "GET "
-- WeirdClient
-- >>> parseRequestLine "GET /NotHTTP UNKNOWN/1.1"
-- WeirdClient
-- >>> parseRequestLine "PRI * HTTP/2.0"
-- WeirdClient
parseRequestLine ::
  ByteString ->
  IO
    ( Http.Method,
      ByteString, -- Path
      ByteString, -- Query
      Http.HttpVersion
    )
parseRequestLine (PS fptr off len) =
  withForeignPtr fptr \ptr -> do
    when (len < 14) (throwIO WeirdClient)

    let methodptr = ptr `plusPtr` off :: Ptr Word8
        limptr = methodptr `plusPtr` len :: Ptr Word8

    pathptr0 <- memchr methodptr 32 (fromIntegral @Int @CSize len) -- ' '
    when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) (throwIO WeirdClient)
    let pathptr = pathptr0 `plusPtr` 1 :: Ptr Word8
        lim1 = limptr `minusPtr` pathptr0

    httpptr0 <- memchr pathptr 32 (fromIntegral @Int @CSize lim1) -- ' '
    when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) (throwIO WeirdClient)
    let httpptr = httpptr0 `plusPtr` 1 :: Ptr Word8
        lim2 = httpptr0 `minusPtr` pathptr

    checkHTTP httpptr
    !hv <- httpVersion httpptr
    queryptr <- memchr pathptr 63 (fromIntegral @Int @CSize lim2) -- '?'
    let !method = bs ptr methodptr pathptr0
        !path
          | queryptr == nullPtr = bs ptr pathptr httpptr0
          | otherwise = bs ptr pathptr queryptr
        !query
          | queryptr == nullPtr = ByteString.empty
          | otherwise = bs ptr queryptr httpptr0

    pure (method, path, query, hv)
  where
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
      w0 <- peek $ p `plusPtr` n
      when (w0 /= w) (throwIO WeirdClient)

    -- H T T P / .
    checkHTTP :: Ptr Word8 -> IO ()
    checkHTTP httpptr = do
      check httpptr 0 72
      check httpptr 1 84
      check httpptr 2 84
      check httpptr 3 80
      check httpptr 4 47
      check httpptr 6 46

    httpVersion :: Ptr Word8 -> IO Http.HttpVersion
    httpVersion httpptr = do
      check httpptr 5 49
      peek @Word8 (httpptr `plusPtr` 7) >>= \case
        49 -> pure Http.http11
        48 -> pure Http.http10
        _ -> throwIO WeirdClient

    bs :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> ByteString
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0

----------------------------------------------------------------

-- >>> parseHeader "Content-Length:47"
-- ("Content-Length","47")
-- >>> parseHeader "Accept-Ranges: bytes"
-- ("Accept-Ranges","bytes")
-- >>> parseHeader "Host:  example.com:8080"
-- ("Host","example.com:8080")
-- >>> parseHeader "NoSemiColon"
-- ("NoSemiColon","")
parseHeader :: ByteString -> Http.Header
parseHeader =
  bimap CI.mk parse . split
  where
    split = ByteString.break (== Byte.colon)
    parse = ByteString.dropWhile (\c -> c == Byte.space || c == Byte.tab) . ByteString.drop 1
