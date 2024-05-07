module Warpless.ChunkedSource
  ( ChunkedSource,
    make,
    read,
  )
where

import Data.ByteString qualified as ByteString
import Warpless.Byte qualified as Byte
import Warpless.ByteString qualified as ByteString (readHex)
import Warpless.Prelude
import Warpless.Source (Source)
import Warpless.Source qualified as Source

data ChunkedSource
  = CSource
      {-# UNPACK #-} !Source
      {-# UNPACK #-} !(IORef ChunkState)

data ChunkState
  = NeedLen
  | NeedLenNewline
  | HaveLen {-# UNPACK #-} !Word
  | DoneChunking

make :: Source -> IO ChunkedSource
make source = do
  chunkStateRef <- newIORef NeedLen
  pure (CSource source chunkStateRef)

read :: ChunkedSource -> IO ByteString
read (CSource source chunkStateRef) = do
  readIORef chunkStateRef >>= \case
    NeedLen -> getLen source chunkStateRef
    NeedLenNewline -> do
      dropCRLF source
      getLen source chunkStateRef
    HaveLen 0 -> do
      -- Drop the final CRLF
      dropCRLF source
      writeIORef chunkStateRef DoneChunking
      pure ByteString.empty
    HaveLen len -> do
      bytes <- Source.read source
      withLen source chunkStateRef len bytes
    DoneChunking -> pure ByteString.empty

getLen :: Source -> IORef ChunkState -> IO ByteString
getLen source chunkStateRef = do
  bytes <- Source.read source
  if ByteString.null bytes
    then do
      writeIORef chunkStateRef (assert False (HaveLen 0))
      pure ByteString.empty
    else do
      (lenBytes, bytes1) <-
        case ByteString.break (== Byte.lf) bytes of
          (lenBytes, bytes1)
            | ByteString.null bytes1 -> do
                bytes2 <- Source.readIgnoringLeftovers source
                pure
                  if ByteString.null bytes2
                    then (lenBytes, bytes1)
                    else ByteString.break (== Byte.lf) $ bytes `ByteString.append` bytes2
            | otherwise -> pure (lenBytes, bytes1)
      let bytes2 = ByteString.drop 1 bytes1
      bytes3 <-
        if ByteString.null bytes2
          then Source.read source
          else pure bytes2
      withLen source chunkStateRef (ByteString.readHex lenBytes) bytes3

withLen :: Source -> IORef ChunkState -> Word -> ByteString -> IO ByteString
withLen source chunkStateRef len bytes
  | len == 0 = do
      Source.setLeftovers source bytes
      dropCRLF source
      writeIORef chunkStateRef DoneChunking
      pure ByteString.empty
  | ByteString.null bytes = do
      -- FIXME should this throw an exception if len > 0?
      writeIORef chunkStateRef DoneChunking
      pure ByteString.empty
  | otherwise =
      case ByteString.length bytes `compare` unsafeFrom @Word @Int len of
        EQ -> do
          writeIORef chunkStateRef NeedLenNewline
          pure bytes
        LT -> do
          writeIORef chunkStateRef (HaveLen (len - unsafeFrom @Int @Word (ByteString.length bytes)))
          pure bytes
        GT -> do
          let (bytes1, bytes2) = ByteString.splitAt (unsafeFrom @Word @Int len) bytes
          Source.setLeftovers source bytes2
          writeIORef chunkStateRef NeedLenNewline
          pure bytes1

dropCRLF :: Source -> IO ()
dropCRLF source = do
  bytes <- Source.read source
  if
    | ByteString.null bytes -> pure ()
    | ByteString.head bytes == Byte.cr -> dropLF source (ByteString.tail bytes)
    | ByteString.head bytes == Byte.lf -> Source.setLeftovers source (ByteString.tail bytes)
    | otherwise -> Source.setLeftovers source bytes

dropLF :: Source -> ByteString -> IO ()
dropLF source bytes
  | ByteString.null bytes = do
      bytes1 <- Source.readIgnoringLeftovers source
      when (not (ByteString.null bytes1)) (dropLF source bytes1)
  | ByteString.head bytes == Byte.lf = Source.setLeftovers source (ByteString.tail bytes)
  | otherwise = Source.setLeftovers source bytes
