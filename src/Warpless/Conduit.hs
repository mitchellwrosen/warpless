module Warpless.Conduit
  ( mkCSource,
    readCSource,
  )
where

import Data.ByteString qualified as ByteString
import Warpless.ByteString qualified as ByteString (readHex)
import Warpless.Prelude
import Warpless.Source (Source, leftoverSource, readSource, readSource')

data CSource
  = CSource
      {-# UNPACK #-} !Source
      {-# UNPACK #-} !(IORef ChunkState)

data ChunkState
  = NeedLen
  | NeedLenNewline
  | HaveLen {-# UNPACK #-} !Word
  | DoneChunking

mkCSource :: Source -> IO CSource
mkCSource source = do
  ref <- newIORef NeedLen
  pure (CSource source ref)

readCSource :: CSource -> IO ByteString
readCSource (CSource src ref) = do
  readIORef ref >>= \case
    NeedLen -> getLen
    NeedLenNewline -> do
      dropCRLF
      getLen
    HaveLen 0 -> do
      -- Drop the final CRLF
      dropCRLF
      writeIORef ref DoneChunking
      pure ByteString.empty
    HaveLen len -> do
      bs <- readSource src
      withLen len bs
    DoneChunking -> pure ByteString.empty
  where
    withLen len bs
      | len == 0 = do
          leftoverSource src bs
          dropCRLF
          yield' ByteString.empty DoneChunking
      | ByteString.null bs = do
          -- FIXME should this throw an exception if len > 0?
          writeIORef ref DoneChunking
          pure ByteString.empty
      | otherwise =
          case ByteString.length bs `compare` unsafeFrom @Word @Int len of
            EQ -> yield' bs NeedLenNewline
            LT -> yield' bs (HaveLen (len - unsafeFrom @Int @Word (ByteString.length bs)))
            GT -> do
              let (x, y) = ByteString.splitAt (unsafeFrom @Word @Int len) bs
              leftoverSource src y
              yield' x NeedLenNewline

    yield' :: ByteString -> ChunkState -> IO ByteString
    yield' bs mlen = do
      writeIORef ref mlen
      pure bs

    dropCRLF = do
      bs <- readSource src
      case ByteString.uncons bs of
        Nothing -> pure ()
        Just (13, bs') -> dropLF bs'
        Just (10, bs') -> leftoverSource src bs'
        Just _ -> leftoverSource src bs

    dropLF bs =
      case ByteString.uncons bs of
        Nothing -> do
          bs2 <- readSource' src
          when (not (ByteString.null bs2)) $ dropLF bs2
        Just (10, bs') -> leftoverSource src bs'
        Just _ -> leftoverSource src bs

    -- Get the length from the source, and then pass off control to withLen
    getLen :: IO ByteString
    getLen = do
      bs <- readSource src
      if ByteString.null bs
        then do
          writeIORef ref $ assert False $ HaveLen 0
          pure ByteString.empty
        else do
          (x, y) <-
            case ByteString.break (== newline) bs of
              (x, y)
                | ByteString.null y -> do
                    bs2 <- readSource' src
                    pure
                      if ByteString.null bs2
                        then (x, y)
                        else ByteString.break (== newline) $ bs `ByteString.append` bs2
                | otherwise -> pure (x, y)
          let y' = ByteString.drop 1 y
          y'' <-
            if ByteString.null y'
              then readSource src
              else pure y'
          withLen (ByteString.readHex x) y''

newline :: Word8
newline =
  10
