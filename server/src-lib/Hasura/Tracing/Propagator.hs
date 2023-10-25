{-# OPTIONS_GHC -Wno-type-defaults #-}

module Hasura.Tracing.Propagator
  ( Propagator (..),
    HttpPropagator,
    extract,
    inject,
  )
where

import Control.Monad.IO.Class
import Hasura.Prelude
import Hasura.Tracing.Context
import Hasura.Tracing.Sampling (samplingStateFromHeader)
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState (emptyTraceState)
import Network.HTTP.Types (RequestHeaders, ResponseHeaders)

-- | A carrier is the medium used by Propagators to read values from and write values to.
-- Each specific Propagator type defines its expected carrier type, such as a string map or a byte array.
data Propagator inboundCarrier outboundCarrier = Propagator
  { extractor :: inboundCarrier -> SpanId -> Maybe TraceContext,
    injector :: TraceContext -> outboundCarrier -> outboundCarrier
  }

instance (Semigroup o) => Semigroup (Propagator i o) where
  (Propagator lExtract lInject) <> (Propagator rExtract rInject) =
    Propagator
      { extractor = \i sid -> lExtract i sid <|> rExtract i sid,
        injector = \c -> lInject c <> rInject c
      }

instance (Semigroup o) => Monoid (Propagator i o) where
  mempty = Propagator (\_ _ -> Nothing) (\_ p -> p)

type HttpPropagator = Propagator RequestHeaders ResponseHeaders

-- | Extracts the value from an incoming request. For example, from the headers of an HTTP request.
--
-- If a value can not be parsed from the carrier, for a cross-cutting concern, the implementation MUST NOT throw an exception and MUST NOT store a new value in the Context, in order to preserve any previously existing valid value.
extract ::
  (MonadIO m) =>
  Propagator i o ->
  -- | The carrier that holds the propagation fields. For example, an incoming message or HTTP request.
  i ->
  -- | a new Context derived from the Context passed as argument, containing the extracted value, which can be a SpanContext, Baggage or another cross-cutting concern context.
  m TraceContext
extract (Propagator extractor _) i = do
  freshSpanId <- randomSpanId
  onNothing (extractor i freshSpanId) (randomContext freshSpanId)
  where
    randomContext freshSpanId = do
      freshTraceId <- randomTraceId
      let samplingState = samplingStateFromHeader Nothing
      pure $ TraceContext freshTraceId freshSpanId Nothing samplingState emptyTraceState

-- | Injects the value into a carrier. For example, into the headers of an HTTP request.
inject ::
  Propagator i o ->
  TraceContext ->
  -- | The carrier that holds the propagation fields. For example, an outgoing message or HTTP request.
  o ->
  o
inject (Propagator _ injector) c = injector c
