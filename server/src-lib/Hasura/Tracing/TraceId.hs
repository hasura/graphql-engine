module Hasura.Tracing.TraceId
  ( -- * TraceId
    TraceId,
    randomTraceId,
    traceIdFromBytes,
    traceIdToBytes,
    traceIdFromHex,
    traceIdToHex,

    -- * SpanId
    SpanId,
    randomSpanId,
    spanIdFromBytes,
    spanIdToBytes,
    spanIdFromHex,
    spanIdToHex,
  )
where

import Data.Aeson qualified as J
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.Serialize qualified as Serialize
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hasura.Prelude
import System.Random.Stateful qualified as Random

--------------------------------------------------------------------------------
-- TraceId

-- | 128-bit trace identifiers.
--
-- 'TraceId's are guaranteed to have at least one non-zero bit.
data TraceId
  = TraceId
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving (Eq)

showTraceId :: TraceId -> T.Text
showTraceId = T.decodeASCII . traceIdToHex

instance J.ToJSON TraceId where
  toJSON = J.toJSON . showTraceId

instance Show TraceId where
  show = T.unpack . showTraceId

-- 128 bits
traceIdBytes :: Int
traceIdBytes = 16

randomTraceId :: (MonadIO m) => m TraceId
randomTraceId = liftIO do
  (w1, w2) <-
    flip Random.applyAtomicGen Random.globalStdGen $ \gen0 ->
      let (!w1, !gen1) = Random.random gen0
          (!w2, !gen2) = Random.random gen1
       in ((w1, w2), gen2)
  if w1 .|. w2 == 0
    then randomTraceId
    else pure $ TraceId w1 w2

-- | Create a 'TraceId' from a 'ByteString'.
--
-- Fails if the 'ByteString' is not exactly 16 bytes long, or if it contains
-- only zero bytes.
traceIdFromBytes :: ByteString -> Maybe TraceId
traceIdFromBytes bs = do
  guard $ ByteString.length bs == traceIdBytes
  (w1, w2) <-
    eitherToMaybe
      $ flip Serialize.runGet bs
      $ (,)
      <$> Serialize.getWord64be
      <*> Serialize.getWord64be
  guard $ w1 .|. w2 /= 0
  pure $ TraceId w1 w2

-- | Convert a 'TraceId' to a 'ByteString' of 16 bytes.
traceIdToBytes :: TraceId -> ByteString
traceIdToBytes (TraceId w1 w2) =
  Serialize.runPut $ Serialize.putWord64be w1 >> Serialize.putWord64be w2

-- | Parse a 'TraceId' from the standard ASCII-encoded hex string format.
--
-- Fails if the 'ByteString' is not exactly 32 characters long, or if it
-- contains only zero characters.
traceIdFromHex :: ByteString -> Maybe TraceId
traceIdFromHex = traceIdFromBytes <=< eitherToMaybe . Base16.decode

-- | Serialize a 'TraceId' to the standard ASCII-encoded hex representation.
traceIdToHex :: TraceId -> ByteString
traceIdToHex = Base16.encode . traceIdToBytes

--------------------------------------------------------------------------------
-- SpanId

-- | 64-bit span identifiers
--
-- 'SpanId's are guaranteed to have at least one non-zero bit.
newtype SpanId = SpanId Word64
  deriving (Eq)

instance J.ToJSON SpanId where
  toJSON = J.toJSON . showSpanId

showSpanId :: SpanId -> T.Text
showSpanId = T.decodeASCII . spanIdToHex

instance Show SpanId where
  show = T.unpack . showSpanId

-- 64 bits
spanIdBytes :: Int
spanIdBytes = 8

randomSpanId :: (MonadIO m) => m SpanId
randomSpanId = liftIO do
  w <- Random.uniformM Random.globalStdGen
  if w == 0
    then randomSpanId
    else pure $ SpanId w

-- | Create a 'SpanId' from a 'ByteString'.
--
-- Fails if the 'ByteString' is not exactly 8 bytes long, or if it contains
-- only zero bytes.
spanIdFromBytes :: ByteString -> Maybe SpanId
spanIdFromBytes bs = do
  guard $ ByteString.length bs == spanIdBytes
  w <- eitherToMaybe $ Serialize.runGet Serialize.getWord64be bs
  guard $ w /= 0
  pure $ SpanId w

-- | Convert a 'SpanId' to a 'ByteString' of 8 bytes.
spanIdToBytes :: SpanId -> ByteString
spanIdToBytes (SpanId w) = Serialize.runPut $ Serialize.putWord64be w

-- | Parse a 'SpanId' from the standard ASCII-encoded hex string format.
--
-- Fails if the 'ByteString' is not exactly 16 characters long, or if it
-- contains only zero characters.
spanIdFromHex :: ByteString -> Maybe SpanId
spanIdFromHex = spanIdFromBytes <=< eitherToMaybe . Base16.decode

-- | Serialize a 'SpanId' to the standard ASCII-encoded hex representation.
spanIdToHex :: SpanId -> ByteString
spanIdToHex = Base16.encode . spanIdToBytes
