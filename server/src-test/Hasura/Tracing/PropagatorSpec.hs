module Hasura.Tracing.PropagatorSpec (spec) where

import Data.Maybe (fromJust)
import Hasura.Prelude
import Hasura.RQL.Types.OpenTelemetry qualified as OTEL
import Hasura.Tracing
import Test.Hspec

spec :: Spec
spec = do
  describe "B3TraceContextPropagator" $ do
    it "extract and inject x-b3 headers" $ do
      traceId <- randomTraceId
      spanId <- randomSpanId
      parentSpanId <- randomSpanId
      tc <-
        extract
          b3TraceContextPropagator
          [ ("X-B3-TraceId", traceIdToHex traceId),
            ("X-B3-SpanId", spanIdToHex spanId),
            ("X-B3-ParentSpanId", spanIdToHex parentSpanId),
            ("X-B3-Sampled", fromJust $ samplingStateToHeader SamplingAccept)
          ]
      tcCurrentTrace tc `shouldBe` traceId
      tcCurrentParent tc `shouldBe` Just spanId
      tcSamplingState tc `shouldBe` SamplingAccept
      tcStateState tc `shouldBe` emptyTraceState

      inject b3TraceContextPropagator tc []
        `shouldBe` [ ("X-B3-TraceId", traceIdToHex traceId),
                     ("X-B3-SpanId", spanIdToHex $ tcCurrentSpan tc),
                     ("X-B3-ParentSpanId", spanIdToHex spanId),
                     ("X-B3-Sampled", fromJust $ samplingStateToHeader SamplingAccept)
                   ]

  describe "W3cTraceContextPropagator" $ do
    it "extract and inject w3c tracecontext headers" $ do
      traceId <- randomTraceId
      spanId <- randomSpanId
      parentSpanId <- randomSpanId
      let headers =
            inject
              w3cTraceContextPropagator
              (TraceContext traceId spanId (Just parentSpanId) SamplingAccept emptyTraceState)
              []
      tc <- extract w3cTraceContextPropagator headers
      tcCurrentTrace tc `shouldBe` traceId
      tcCurrentParent tc `shouldBe` Just spanId
      tcSamplingState tc `shouldBe` SamplingAccept
      tcStateState tc `shouldBe` emptyTraceState

  describe "Composite Propagator" $ do
    it "extract and inject propagator b3 + w3c" $ do
      traceId1 <- randomTraceId
      spanId1 <- randomSpanId
      parentSpanId1 <- randomSpanId
      traceId2 <- randomTraceId
      spanId2 <- randomSpanId
      parentSpanId2 <- randomSpanId
      let propagator = OTEL.mkOtelTracesPropagator [OTEL.TraceContext, OTEL.B3]
          headers =
            ( inject
                b3TraceContextPropagator
                (TraceContext traceId1 spanId1 (Just parentSpanId1) SamplingAccept emptyTraceState)
                []
            )
              <> ( inject
                     w3cTraceContextPropagator
                     (TraceContext traceId2 spanId2 (Just parentSpanId2) SamplingDefer emptyTraceState)
                     []
                 )
      tc <- extract propagator headers
      tcCurrentTrace tc `shouldBe` traceId2
      tcCurrentParent tc `shouldBe` Just spanId2
      tcSamplingState tc `shouldBe` SamplingDefer
      tcStateState tc `shouldBe` emptyTraceState

      inject propagator tc []
        `shouldBe` (inject w3cTraceContextPropagator tc [])
          <> (inject b3TraceContextPropagator tc [])
