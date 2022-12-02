module Hasura.Tracing.TraceIdSpec (spec) where

import Hasura.Prelude
import Hasura.Tracing.TraceId
  ( randomSpanId,
    randomTraceId,
    spanIdFromBytes,
    spanIdToBytes,
    traceIdFromBytes,
    traceIdToBytes,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "TraceId" $ do
    it "roundtrips to and from ByteString" $ do
      let randomRoundTrip :: IO Bool
          randomRoundTrip =
            randomTraceId <&> \traceId ->
              traceIdFromBytes (traceIdToBytes traceId) == Just traceId
      fmap and (replicateM 100 randomRoundTrip) `shouldReturn` True
  describe "SpanId" $ do
    it "roundtrips to and from ByteString" $ do
      let randomRoundTrip :: IO Bool
          randomRoundTrip =
            randomSpanId <&> \spanId ->
              spanIdFromBytes (spanIdToBytes spanId) == Just spanId
      fmap and (replicateM 100 randomRoundTrip) `shouldReturn` True
