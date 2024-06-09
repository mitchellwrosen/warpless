module Warpless.WriteBuffer
  ( WriteBuffer (..),
    allocate,
    toBuilderBuffer,
  )
where

import Data.Streaming.ByteString.Builder.Buffer qualified as B (Buffer (..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Warpless.Prelude

data WriteBuffer = WriteBuffer
  { buffer :: {-# UNPACK #-} !(Ptr Word8),
    size :: {-# UNPACK #-} !Int
  }

allocate :: Int -> IO WriteBuffer
allocate size = do
  buffer <- mallocBytes size
  pure WriteBuffer {buffer, size}

toBuilderBuffer :: WriteBuffer -> IO B.Buffer
toBuilderBuffer (WriteBuffer buf size) = do
  fbuf <- newForeignPtr_ buf
  pure (B.Buffer fbuf buf buf (buf `plusPtr` size))
