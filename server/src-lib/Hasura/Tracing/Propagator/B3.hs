-- | B3 Propagation is a specification for the header "b3" and those that start with "x-b3-".
-- These headers are used for trace context propagation across service boundaries.
-- https://github.com/openzipkin/b3-propagation
-- https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/context/api-propagators.md#b3-requirements
module Hasura.Tracing.Propagator.B3
  ( b3TraceContextPropagator,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Hasura.Prelude
import Hasura.Tracing.Context (TraceContext (..))
import Hasura.Tracing.Propagator (Propagator (..))
import Hasura.Tracing.Sampling (samplingStateFromHeader, samplingStateToHeader)
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState (emptyTraceState)
import Network.HTTP.Client.Transformable (RequestHeaders, ResponseHeaders)

b3TraceContextPropagator :: Propagator RequestHeaders ResponseHeaders
b3TraceContextPropagator =
  Propagator
    { extractor = extractB3TraceContext,
      injector = \TraceContext {..} headers ->
        headers
          ++ catMaybes
            [ Just ("X-B3-TraceId", traceIdToHex tcCurrentTrace),
              Just ("X-B3-SpanId", spanIdToHex tcCurrentSpan),
              ("X-B3-ParentSpanId",) . spanIdToHex <$> tcCurrentParent,
              ("X-B3-Sampled",) <$> samplingStateToHeader tcSamplingState
            ]
    }

extractB3TraceContext :: RequestHeaders -> SpanId -> Maybe TraceContext
extractB3TraceContext headers freshSpanId = do
  -- B3 TraceIds can have a length of either 64 bits (16 hex chars) or 128 bits
  -- (32 hex chars). For 64-bit TraceIds, we pad them with zeros on the left to
  -- make them 128 bits long.
  traceId <-
    lookup "X-B3-TraceId" headers >>= \rawTraceId ->
      if
        | Char8.length rawTraceId == 32 ->
            traceIdFromHex rawTraceId
        | Char8.length rawTraceId == 16 ->
            traceIdFromHex $ Char8.replicate 16 '0' <> rawTraceId
        | otherwise ->
            Nothing
  let parentSpanId = spanIdFromHex =<< lookup "X-B3-SpanId" headers
      samplingState = samplingStateFromHeader $ lookup "X-B3-Sampled" headers
  Just $ TraceContext traceId freshSpanId parentSpanId samplingState emptyTraceState
