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
              dimensions = RequestDimensions Hit Mutation Local HTTP
            , bucket = RunningTimeBucket {bucketGreaterThan =        0}
            , metrics = RequestTimingsCount {telemTimeIO = 2, telemTimeTot = 200, telemCount = 2}},
          ServiceTimingMetric {
              dimensions = RequestDimensions Miss Mutation Local HTTP
            , bucket = RunningTimeBucket {bucketGreaterThan      = 1000}
            , metrics = RequestTimingsCount {telemTimeIO = 2, telemTimeTot = 2002, telemCount = 2}},
          ServiceTimingMetric {
              dimensions = RequestDimensions Hit Query Remote WebSocket
            , bucket = RunningTimeBucket {bucketGreaterThan      = 100000}
            , metrics = RequestTimingsCount {telemTimeIO = 1, telemTimeTot = 100001, telemCount = 1}}]

    it "accumulates as expected" $ do
      recordTimingMetric (RequestDimensions Hit  Mutation Local  HTTP) (RequestTimings 1 100)
      recordTimingMetric (RequestDimensions Hit  Mutation Local  HTTP) (RequestTimings 1 100)
      recordTimingMetric (RequestDimensions Miss Mutation Local  HTTP) (RequestTimings 1 1001)
      recordTimingMetric (RequestDimensions Miss Mutation Local  HTTP) (RequestTimings 1 1001)
      recordTimingMetric (RequestDimensions Hit  Query    Remote WebSocket) (RequestTimings 1 100001)
      fmap serviceTimingMetrics dumpServiceTimingMetrics `shouldReturn` expected

    it "serializes and deserializes properly" $ do
      fmap (fmap serviceTimingMetrics . A.eitherDecode . A.encode) dumpServiceTimingMetrics
        `shouldReturn` Right expected
