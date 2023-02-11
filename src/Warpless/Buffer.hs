module Warpless.Buffer
  ( bufferIO,
  )
where

import Data.ByteString.Internal (ByteString (..))
import Foreign.ForeignPtr
import Network.Socket.BufferPool

bufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO ptr siz io = do
  fptr <- newForeignPtr_ ptr
  io $ PS fptr 0 siz
