module Histogram (tests) where

import qualified Data.Map.Strict as Map
import System.Metrics.Prometheus.Histogram (HistogramSample (..), histLastBucketCount)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import Test.HUnit (assertEqual)
import Test.Hspec

tests :: Spec
tests =
  describe "Histogram" $ do
    it "passes a basic unit test, checking buckets and invariants" test_basicUnit

test_basicUnit :: IO ()
test_basicUnit = do
  let buckets = [1, 3, 5]
      -- to be added to histogram, expected in...
      items =
        [ -1, -- ...bucket 1
          4, -- ...bucket 5
          6, -- ...bucket inf
          7, -- ...bucket inf
          0, -- ...bucket 1
          5, -- ...bucket 5
          100 -- ...bucket inf
        ]
      expectedExplicitBuckets = (Map.fromList [(1, 2), (3, 0), (5, 2)])
      expectedOverflowBucketCount = 3

  hist <- Histogram.new buckets
  mapM_ (Histogram.observe hist) items
  histSample <- Histogram.read hist

  assertEqual "has everything in expected explicit buckets" (histBuckets histSample) expectedExplicitBuckets
  assertEqual "...and in last overflow bucket" (histLastBucketCount histSample) expectedOverflowBucketCount
  assertEqual "total count invariant" (histCount histSample) (fromIntegral (sum $ histBuckets histSample) + histLastBucketCount histSample)
  assertEqual "sum is consistent" (histSum histSample) (sum items)
