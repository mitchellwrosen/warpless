module Warpless.IO
  ( toBufIOWith,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (BufferWriter, Next (Chunk, Done, More), runBuilder)
import Data.ByteString.Internal (ByteString (PS))
import Foreign.ForeignPtr (newForeignPtr_)
import Warpless.Prelude
import Warpless.WriteBuffer (WriteBuffer (..), createWriteBuffer, freeWriteBuffer)

toBufIOWith :: IORef WriteBuffer -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith writeBufferRef io builder = do
  writeBuffer <- readIORef writeBufferRef
  loop writeBuffer (runBuilder builder)
  where
    loop :: WriteBuffer -> BufferWriter -> IO ()
    loop writeBuffer writer = do
      let buf = bufBuffer writeBuffer
          size = bufSize writeBuffer
      (len, signal) <- writer buf size
      fptr <- newForeignPtr_ buf
      io (PS fptr 0 len)
      case signal of
        Done -> pure ()
        More minSize next
          | size < minSize -> do
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
                  pure biggerWriteBuffer
              loop biggerWriteBuffer next
          | otherwise -> loop writeBuffer next
        Chunk bs next -> do
          io bs
          loop writeBuffer next
