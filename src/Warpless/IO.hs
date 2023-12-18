module Warpless.IO
  ( toBufIOWith,
  )
where

import Control.Exception (mask_)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (BufferWriter, Next (Chunk, Done, More), runBuilder)
import Data.ByteString.Internal (ByteString (PS))
import Data.IORef (IORef, readIORef, writeIORef)
import Foreign.ForeignPtr (newForeignPtr_)
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer, freeWriteBuffer)

toBufIOWith :: Int -> IORef WriteBuffer -> (ByteString -> IO ()) -> Builder -> IO Int
toBufIOWith maxRspBufSize writeBufferRef io builder = do
  writeBuffer <- readIORef writeBufferRef
  loop writeBuffer (runBuilder builder) 0
  where
    loop :: WriteBuffer -> BufferWriter -> Int -> IO Int
    loop writeBuffer writer bytesSent = do
      let buf = bufBuffer writeBuffer
          size = bufSize writeBuffer
      (len, signal) <- writer buf size
      fptr <- newForeignPtr_ buf
      io (PS fptr 0 len)
      let totalBytesSent = len + bytesSent
      case signal of
        Done -> pure totalBytesSent
        More minSize next
          | size < minSize -> do
              when (minSize > maxRspBufSize) $
                error $
                  "Sending a Builder response required a buffer of size "
                    ++ show minSize
                    ++ " which is bigger than the specified maximum of "
                    ++ show maxRspBufSize
                    ++ "!"
              -- The current WriteBuffer is too small to fit the next
              -- batch of bytes from the Builder so we free it and
              -- create a new bigger one. Freeing the current buffer,
              -- creating a new one and writing it to the IORef need
              -- to be performed atomically to prevent both double
              -- frees and missed frees. So we mask async exceptions:
              biggerWriteBuffer <-
                mask_ do
                  freeWriteBuffer writeBuffer
                  biggerWriteBuffer <- createWriteBuffer minSize
                  writeIORef writeBufferRef biggerWriteBuffer
                  return biggerWriteBuffer
              loop biggerWriteBuffer next totalBytesSent
          | otherwise -> loop writeBuffer next totalBytesSent
        Chunk bs next -> do
          io bs
          loop writeBuffer next totalBytesSent
