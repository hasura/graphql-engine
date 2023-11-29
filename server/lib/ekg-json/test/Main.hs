{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (encode)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Exts (fromString)
import GHC.TypeLits (Symbol)
import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Json
import qualified System.Metrics.Label as Label
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    shouldBe,
  )

main :: IO ()
main = hspec exampleSpec

-- | Test whether the output of 'sampleToJson' is consistent with its
-- Haddocks.
--
-- Note: The example output in the Haddocks has been formatted by
-- passing it through the `jq` program.
exampleSpec :: Spec
exampleSpec =
  describe "The output of `sampleToJson`" $
    it "is consistent with its haddocks" $ do
      store <- newStore @ExampleMetrics

      counter1 <-
        createCounter ExampleCounter (HashMap.singleton "key1" "val1") store
      Counter.add counter1 10

      counter2 <-
        createCounter ExampleCounter (HashMap.singleton "key1" "val2") store
      Counter.add counter2 11

      distribution <- createDistribution ExampleDistribution () store
      Distribution.add distribution 1

      gauge <- createGauge ExampleGauge () store
      Gauge.set gauge 100

      label <- createLabel ExampleLabel () store
      Label.set label "bar"

      jsonSample <- encode . sampleToJson <$> sampleAll store

      shouldBe jsonSample "{\"foo\":{\"counter\":[{\"tags\":{\"key1\":\"val1\"},\"value\":{\"type\":\"c\",\"val\":10}},{\"tags\":{\"key1\":\"val2\"},\"value\":{\"type\":\"c\",\"val\":11}}],\"distribution\":[{\"tags\":{},\"value\":{\"count\":1,\"max\":1,\"mean\":1,\"min\":1,\"sum\":1,\"type\":\"d\",\"variance\":0}}]},\"gauge\":[{\"tags\":{},\"value\":{\"type\":\"g\",\"val\":100}}],\"label\":[{\"tags\":{},\"value\":{\"type\":\"l\",\"val\":\"bar\"}}]}"

data ExampleMetrics :: Symbol -> MetricType -> Type -> Type where
  ExampleCounter ::
    ExampleMetrics "foo.counter" 'CounterType (HashMap.HashMap T.Text T.Text)
  ExampleDistribution ::
    ExampleMetrics "foo.distribution" 'DistributionType ()
  ExampleGauge ::
    ExampleMetrics "gauge" 'GaugeType ()
  ExampleLabel ::
    ExampleMetrics "label" 'LabelType ()
