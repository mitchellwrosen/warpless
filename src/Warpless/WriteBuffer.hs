module Warpless.WriteBuffer
  ( WriteBuffer (bufBuffer, bufSize),
    createWriteBuffer,
    freeWriteBuffer,
    toBuilderBuffer,
  )
where

import Data.Streaming.ByteString.Builder.Buffer qualified as B (Buffer (..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (plusPtr)
import Network.Socket.BufferPool (BufSize, Buffer)
import Warpless.Prelude

-- | A write buffer of a specified size containing bytes and a way to free the buffer.
data WriteBuffer = WriteBuffer
  { bufBuffer :: !Buffer,
    -- | The size of the write buffer.
    bufSize :: !BufSize,
    -- | Free the allocated buffer. Warp guarantees it will only be called once, and no other functions will be called
    -- after it.
    bufFree :: !(IO ())
  }

-- | Allocate a buffer of the given size and wrap it in a 'WriteBuffer'
-- containing that size and a finalizer.
createWriteBuffer :: BufSize -> IO WriteBuffer
createWriteBuffer size = do
  bytes <- mallocBytes size
  pure
    WriteBuffer
      { bufBuffer = bytes,
        bufSize = size,
        bufFree = free bytes
      }

freeWriteBuffer :: WriteBuffer -> IO ()
freeWriteBuffer WriteBuffer {bufFree} =
  bufFree

toBuilderBuffer :: WriteBuffer -> IO B.Buffer
toBuilderBuffer writeBuffer = do
  let ptr = bufBuffer writeBuffer
      size = bufSize writeBuffer
  fptr <- newForeignPtr_ ptr
  pure (B.Buffer fptr ptr ptr (ptr `plusPtr` size))
