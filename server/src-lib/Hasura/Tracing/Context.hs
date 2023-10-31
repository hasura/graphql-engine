module Hasura.Tracing.Context
  ( TraceContext (..),
    TraceMetadata,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Hasura.Prelude
import Hasura.Tracing.Sampling
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState (TraceState)

-- | Any additional human-readable key-value pairs relevant to the execution of
-- a span.
--
-- When the Open Telemetry exporter is in use these become attributes. Where
-- possible and appropriate, consider using key names from the documented OT
-- semantic conventions here:
-- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/
-- This can serve to document the metadata, even for users not using open telemetry.
--
-- We may make this type more closely align with the OT data model in the future
-- (e.g. supporting int, etc)
type TraceMetadata = [(Text, Text)]

-- | A trace context records the current active trace, the active span
-- within that trace, and the span's parent, unless the current span
-- is the root. This is like a call stack.
data TraceContext = TraceContext
  { tcCurrentTrace :: TraceId,
    tcCurrentSpan :: SpanId,
    tcCurrentParent :: Maybe SpanId,
    tcSamplingState :: SamplingState,
    -- Optional vendor-specific trace identification information across different distributed tracing systems.
    -- It's used for the W3C Trace Context only https://www.w3.org/TR/trace-context/#tracestate-header
    tcStateState :: TraceState
  }

-- Should this be here? This implicitly ties Tracing to the name of fields in HTTP headers.
instance J.ToJSON TraceContext where
  toJSON TraceContext {..} =
    let idFields =
          [ "trace_id" .= bsToTxt (traceIdToHex tcCurrentTrace),
            "span_id" .= bsToTxt (spanIdToHex tcCurrentSpan)
          ]
        samplingFieldMaybe =
          samplingStateToHeader @Text tcSamplingState <&> \t ->
            "sampling_state" .= t
     in J.object $ idFields ++ maybeToList samplingFieldMaybe
