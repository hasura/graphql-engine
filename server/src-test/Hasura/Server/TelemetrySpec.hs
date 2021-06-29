{-# LANGUAGE DuplicateRecordFields #-}
module Hasura.Server.TelemetrySpec (spec) where

import           Hasura.Prelude

import qualified Data.Aeson                       as A
import           Hasura.Server.Telemetry.Counters
import           Test.Hspec

spec :: Spec
spec = do
  telemetryCountersTests

-- NOTE: this test is effectful/stateful; if we need to we can implement an
-- operation to clear metric store.
telemetryCountersTests :: Spec
telemetryCountersTests = do
  describe "request timing counters" $ do
    it "is at first empty" $ do
      fmap serviceTimingMetrics dumpServiceTimingMetrics `shouldReturn` []

    -- excercise accumulating and buckets:
    let expected =
          -- NOTE: ordering is arbitrary here (and hence fragile)
          [ServiceTimingMetric {
              dimensions = RequestDimensions Miss Mutation Local HTTP
            , bucket = RunningTimeBucket {bucketGreaterThan      = 0.050}
            , metrics = RequestTimingsCount {telemTimeIO = 2, telemTimeTot = 1.050, telemCount = 2}},
          ServiceTimingMetric {
              dimensions = RequestDimensions Hit Mutation Local HTTP
            , bucket = RunningTimeBucket {bucketGreaterThan =        0}
            , metrics = RequestTimingsCount {telemTimeIO = 2, telemTimeTot = 0.001, telemCount = 2}},
          ServiceTimingMetric {
              dimensions = RequestDimensions Hit Query Remote WebSocket
            , bucket = RunningTimeBucket {bucketGreaterThan      = 1.000}
            , metrics = RequestTimingsCount {telemTimeIO = 1, telemTimeTot = 5.000, telemCount = 1}}]

    it "accumulates as expected" $ do
      -- bucket 0sec - 1ms:
      recordTimingMetric (RequestDimensions Hit  Mutation Local  HTTP)      (RequestTimings 1 0.0001)
      recordTimingMetric (RequestDimensions Hit  Mutation Local  HTTP)      (RequestTimings 1 0.0009)
      -- bucket 50ms - 1 sec:
      recordTimingMetric (RequestDimensions Miss Mutation Local  HTTP)      (RequestTimings 1 0.0510)
      recordTimingMetric (RequestDimensions Miss Mutation Local  HTTP)      (RequestTimings 1 0.9990)
      -- bucket 1 sec - 1 hour:
      recordTimingMetric (RequestDimensions Hit  Query    Remote WebSocket) (RequestTimings 1 5.0000)
      fmap serviceTimingMetrics dumpServiceTimingMetrics `shouldReturn` expected

    it "serializes and deserializes properly" $ do
      fmap (fmap serviceTimingMetrics . A.eitherDecode . A.encode) dumpServiceTimingMetrics
        `shouldReturn` Right expected
