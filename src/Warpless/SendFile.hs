{-# LANGUAGE CPP #-}

module Warpless.SendFile
  ( sendFile,
    readSendFile,
    packHeader, -- for testing
    positionRead,
  )
where

import Data.ByteString qualified as BS
import Foreign.C.Error (throwErrno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Network.Sendfile
import Network.Socket (Socket)
import Network.Socket.BufferPool
import System.Posix.Types
import UnliftIO qualified
import Warpless.Buffer
import Warpless.FdCache (closeFile, openFile)
import Warpless.Imports
import Warpless.Types

----------------------------------------------------------------

-- | Function to send a file based on sendfile() for Linux\/Mac\/FreeBSD.
--   This makes use of the file descriptor cache.
--   For other OSes, this is identical to 'readSendFile'.
--
-- Since: 3.1.0
sendFile :: Socket -> Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
#ifdef SENDFILEFD
sendFile s _ _ _ fid off len act hdr = case mfid of
    -- settingsFdCacheDuration is 0
    Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
  where
    mfid = fileIdFd fid
    path = fileIdPath fid
#else
sendFile _ = readSendFile
#endif

----------------------------------------------------------------

packHeader ::
  Buffer ->
  BufSize ->
  (ByteString -> IO ()) ->
  IO () ->
  [ByteString] ->
  Int ->
  IO Int
packHeader _ _ _ _ [] n = return n
packHeader buf siz send hook (bs : bss) n
  | len < room = do
      let dst = buf `plusPtr` n :: Ptr Word8
      _ <- copy dst bs
      packHeader buf siz send hook bss (n + len)
  | otherwise = do
      let dst = buf `plusPtr` n :: Ptr Word8
          (bs1, bs2) = BS.splitAt room bs
      _ <- copy dst bs1
      bufferIO buf siz send
      hook
      packHeader buf siz send hook (bs2 : bss) 0
  where
    len = BS.length bs
    room = siz - n

mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise = fromIntegral n

-- | Function to send a file based on pread()\/send() for Unix.
--   This makes use of the file descriptor cache.
--   For Windows, this is emulated by 'Handle'.
--
-- Since: 3.1.0
readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers =
  UnliftIO.bracket setup teardown $ \fd -> do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn :: Ptr Word8
    n <- positionRead fd buf' (mini room len0) off0
    bufferIO buf (hn + n) send
    hook
    let n' = fromIntegral @Int @Integer n
    loop fd (len0 - n') (off0 + n')
  where
    path = fileIdPath fid
    setup = case fileIdFd fid of
      Just fd -> return fd
      Nothing -> openFile path
    teardown fd = case fileIdFd fid of
      Just _ -> return ()
      Nothing -> closeFile fd
    loop fd len off
      | len <= 0 = return ()
      | otherwise = do
          n <- positionRead fd buf (mini siz len) off
          bufferIO buf n send
          let n' = fromIntegral @Int @Integer n
          hook
          loop fd (len - n') (off + n')

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
  bytes <- fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
  when (bytes < 0) $ throwErrno "positionRead"
  return bytes

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
