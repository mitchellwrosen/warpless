module Warpless.Conduit
  ( ISource (..),
    mkISource,
    readISource,
    mkCSource,
    readCSource,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef
import Data.Word (Word8)
import UnliftIO (assert, throwIO)
import Warpless.ByteString qualified as ByteString
import Warpless.Source (Source, leftoverSource, readSource, readSource')
import Warpless.Types

----------------------------------------------------------------

-- | Contains a @Source@ and a byte count that is still to be read in.
data ISource = ISource !Source !(IORef Int)

mkISource :: Source -> Int -> IO ISource
mkISource src cnt = do
  ref <- newIORef cnt
  pure (ISource src ref)

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
readISource :: ISource -> IO ByteString
readISource (ISource src ref) = do
  count <- readIORef ref
  if count == 0
    then pure ByteString.empty
    else do
      bs <- readSource src

      -- If no chunk available, then there aren't enough bytes in the
      -- stream. Throw a ConnectionClosedByPeer
      when (ByteString.null bs) (throwIO ConnectionClosedByPeer)

      -- How many of the bytes in this chunk to send downstream
      let toSend = min count (ByteString.length bs)
      -- How many bytes will still remain to be sent downstream
      let count' = count - toSend
      if count' > 0
        then do
          -- The expected count is greater than the size of the
          -- chunk we just read. Send the entire chunk
          -- downstream, and then loop on this function for the
          -- next chunk.
          writeIORef ref count'
          pure bs
        else do
          -- Some of the bytes in this chunk should not be sent
          -- downstream. Split up the chunk into the sent and
          -- not-sent parts, add the not-sent parts onto the new
          -- source, and send the rest of the chunk downstream.
          let (x, y) = ByteString.splitAt toSend bs
          leftoverSource src y
          assert (count' == 0) $ writeIORef ref count'
          pure x

----------------------------------------------------------------

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
mkCSource src = do
  ref <- newIORef NeedLen
  pure (CSource src ref)

readCSource :: CSource -> IO ByteString
readCSource (CSource src ref) = do
  readIORef ref >>= go
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
          case ByteString.length bs `compare` fromIntegral len of
            EQ -> yield' bs NeedLenNewline
            LT -> yield' bs (HaveLen (len - fromIntegral (ByteString.length bs)))
            GT -> do
              let (x, y) = ByteString.splitAt (fromIntegral len) bs
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

    go :: ChunkState -> IO ByteString
    go = \case
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
