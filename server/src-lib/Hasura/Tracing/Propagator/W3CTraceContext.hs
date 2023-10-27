-- | This module provides support for tracing context propagation in accordance with the W3C tracing context
-- propagation specifications: https://www.w3.org/TR/trace-context/
module Hasura.Tracing.Propagator.W3CTraceContext
  ( w3cTraceContextPropagator,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser, hexadecimal, parseOnly, string, takeWhile)
import Data.Bits (Bits (setBit, testBit))
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Word (Word8)
import Hasura.Prelude hiding (takeWhile)
import Hasura.Tracing.Context (TraceContext (..))
import Hasura.Tracing.Propagator (Propagator (..))
import Hasura.Tracing.Sampling (SamplingState (SamplingAccept, SamplingDefer))
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState (decodeTraceStateHeader)
import Hasura.Tracing.TraceState qualified as TS
import Network.HTTP.Types (RequestHeaders, ResponseHeaders)

-- | Propagate trace context information via headers using the w3c specification format
w3cTraceContextPropagator :: Propagator RequestHeaders ResponseHeaders
w3cTraceContextPropagator =
  Propagator
    { extractor = \headers freshSpanId -> do
        TraceParent {..} <- lookup "traceparent" headers >>= decodeTraceparentHeader
        let traceState = lookup "tracestate" headers >>= decodeTraceStateHeader
        Just
          $ TraceContext
            tpTraceId
            freshSpanId
            (Just tpParentId)
            (traceFlagsToSampling tpTraceFlags)
            (fromMaybe TS.emptyTraceState traceState),
      injector = \context headers ->
        let (traceParent, traceState) = encodeSpanContext context
         in headers
              ++ [ ("traceparent", traceParent),
                   ("tracestate", traceState)
                 ]
    }

--------------------------------------------------------------------------------
-- TraceParent

-- | The traceparent HTTP header field identifies the incoming request in a tracing system.
-- https://w3c.github.io/trace-context/#traceparent-header
data TraceParent = TraceParent
  { tpVersion :: {-# UNPACK #-} !Word8,
    tpTraceId :: {-# UNPACK #-} !TraceId,
    tpParentId :: {-# UNPACK #-} !SpanId,
    tpTraceFlags :: {-# UNPACK #-} !TraceFlags
  }
  deriving (Show)

-- | Contain details about the trace. Unlike TraceState values, TraceFlags are present in all traces.
-- The current version of the specification only supports a single flag called sampled.
newtype TraceFlags = TraceFlags Word8
  deriving (Show, Eq, Ord)

-- | TraceFlags with the @sampled@ flag not set. This means that it is up to the
-- sampling configuration to decide whether or not to sample the trace.
defaultTraceFlags :: TraceFlags
defaultTraceFlags = TraceFlags 0

-- | Get the current bitmask for the @TraceFlags@, useful for serialization purposes.
traceFlagsValue :: TraceFlags -> Word8
traceFlagsValue (TraceFlags flags) = flags

-- | Will the trace associated with this @TraceFlags@ value be sampled?
isSampled :: TraceFlags -> Bool
isSampled (TraceFlags flags) = flags `testBit` 0

-- | Set the @sampled@ flag on the @TraceFlags@
setSampled :: TraceFlags -> TraceFlags
setSampled (TraceFlags flags) = TraceFlags (flags `setBit` 0)

traceFlagsToSampling :: TraceFlags -> SamplingState
traceFlagsToSampling = bool SamplingDefer SamplingAccept . isSampled

traceFlagsFromSampling :: SamplingState -> TraceFlags
traceFlagsFromSampling = \case
  SamplingAccept -> setSampled defaultTraceFlags
  _ -> defaultTraceFlags

-- | Encoded the given 'TraceContext' into a @traceparent@, @tracestate@ tuple.
encodeSpanContext :: TraceContext -> (ByteString, ByteString)
encodeSpanContext TraceContext {..} = (traceparent, tracestate)
  where
    traceparent =
      L.toStrict
        $ B.toLazyByteString
        -- version
        $ B.word8HexFixed 0
        <> B.char7 '-'
        <> B.byteString (traceIdToHex tcCurrentTrace)
        <> B.char7 '-'
        <> B.byteString (spanIdToHex tcCurrentSpan)
        <> B.char7 '-'
        <> B.word8HexFixed (traceFlagsValue $ traceFlagsFromSampling tcSamplingState)

    tracestate =
      txtToBs
        $ T.intercalate ","
        $ (\(TS.Key key, TS.Value value) -> key <> "=" <> value)
        <$> (TS.toTraceStateList tcStateState)

traceparentParser :: Parser TraceParent
traceparentParser = do
  tpVersion <- hexadecimal
  _ <- string "-"
  traceIdBs <- takeWhile isHexDigit
  tpTraceId <- onNothing (traceIdFromHex traceIdBs) (fail "TraceId must be 8 bytes long")
  _ <- string "-"
  parentIdBs <- takeWhile isHexDigit
  tpParentId <- onNothing (spanIdFromHex parentIdBs) (fail "ParentId must be 8 bytes long")
  _ <- string "-"
  tpTraceFlags <- TraceFlags <$> hexadecimal
  -- Intentionally not consuming end of input in case of version > 0
  pure $ TraceParent {..}

decodeTraceparentHeader :: ByteString -> Maybe TraceParent
decodeTraceparentHeader tp = case parseOnly traceparentParser tp of
  Left _ -> Nothing
  Right ok -> Just ok
