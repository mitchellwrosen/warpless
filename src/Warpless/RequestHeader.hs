module Warpless.RequestHeader
  ( parseHeaderLines,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Char8 qualified as C8 (unpack)
import Data.ByteString.Internal (ByteString (..), memchr)
import Data.CaseInsensitive qualified as CI
import Data.Word (Word8)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import Network.HTTP.Types qualified as H
import UnliftIO (throwIO)
import Warpless.Types

parseHeaderLines ::
  [ByteString] ->
  IO
    ( H.Method,
      ByteString, --  Path
      ByteString, --  Path, parsed
      ByteString, --  Query
      H.HttpVersion,
      H.RequestHeaders
    )
parseHeaderLines [] = throwIO $ NotEnoughLines []
parseHeaderLines (firstLine : otherLines) = do
  (method, path', query, httpversion) <- parseRequestLine firstLine
  let path = H.extractPath path'
      hdr = map parseHeader otherLines
  return (method, path', path, query, httpversion, hdr)

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "GET "
-- *** Exception: Warp: Invalid first line of request: "GET "
-- >>> parseRequestLine "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: Warp: Request line specified a non-HTTP request
-- >>> parseRequestLine "PRI * HTTP/2.0"
-- ("PRI","*","",HTTP/2.0)
parseRequestLine ::
  ByteString ->
  IO
    ( H.Method,
      ByteString, -- Path
      ByteString, -- Query
      H.HttpVersion
    )
parseRequestLine requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
  when (len < 14) $ throwIO baderr
  let methodptr = ptr `plusPtr` off :: Ptr Word8
      limptr = methodptr `plusPtr` len :: Ptr Word8

  pathptr0 <- memchr methodptr 32 (fromIntegral @Int @CSize len) -- ' '
  when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) $
    throwIO baderr
  let pathptr = pathptr0 `plusPtr` 1 :: Ptr Word8
      lim1 = limptr `minusPtr` pathptr0

  httpptr0 <- memchr pathptr 32 (fromIntegral @Int @CSize lim1) -- ' '
  when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) $
    throwIO baderr
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
        | queryptr == nullPtr = S.empty
        | otherwise = bs ptr queryptr httpptr0

  return (method, path, query, hv)
  where
    baderr = BadFirstLine $ C8.unpack requestLine
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
      w0 <- peek $ p `plusPtr` n
      when (w0 /= w) $ throwIO NonHttp
    checkHTTP httpptr = do
      check httpptr 0 72 -- 'H'
      check httpptr 1 84 -- 'T'
      check httpptr 2 84 -- 'T'
      check httpptr 3 80 -- 'P'
      check httpptr 4 47 -- '/'
      check httpptr 6 46 -- '.'
    httpVersion :: Ptr Word8 -> IO H.HttpVersion
    httpVersion httpptr = do
      major <- peek (httpptr `plusPtr` 5) :: IO Word8
      minor <- peek (httpptr `plusPtr` 7) :: IO Word8
      let version
            | major == 49 = if minor == 49 then H.http11 else H.http10
            | major == 50 && minor == 48 = H.HttpVersion 2 0
            | otherwise = H.http10
      return version
    bs :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> ByteString
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0

----------------------------------------------------------------

-- |
--
-- >>> parseHeader "Content-Length:47"
-- ("Content-Length","47")
-- >>> parseHeader "Accept-Ranges: bytes"
-- ("Accept-Ranges","bytes")
-- >>> parseHeader "Host:  example.com:8080"
-- ("Host","example.com:8080")
-- >>> parseHeader "NoSemiColon"
-- ("NoSemiColon","")
parseHeader :: ByteString -> H.Header
parseHeader s =
  let (k, rest) = S.break (== 58) s -- ':'
      rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
   in (CI.mk k, rest')
