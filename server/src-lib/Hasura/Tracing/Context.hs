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

-- | Any additional human-readable key-value pairs relevant
-- to the execution of a block of code.
type TraceMetadata = [(Text, Text)]

-- | A trace context records the current active trace, the active span
-- within that trace, and the span's parent, unless the current span
-- is the root.
data TraceContext = TraceContext
  { tcCurrentTrace :: TraceId,
    tcCurrentSpan :: SpanId,
    tcCurrentParent :: Maybe SpanId,
    tcSamplingState :: SamplingState
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
