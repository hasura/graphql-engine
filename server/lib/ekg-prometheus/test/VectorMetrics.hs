{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module VectorMetrics where

import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.CounterVector as CounterVector
import System.Metrics.Prometheus.Export (sampleToPrometheus)
import qualified System.Metrics.Prometheus.GaugeVector as GaugeVector
import qualified System.Metrics.Prometheus.HistogramVector as HistogramVector
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

tests :: Spec
tests =
  describe "The output of `sampleToPrometheus` for Vector metrics" $ do
    it "is adding new labels (for counter) while sampling" $ do
      store <- newStore @ExampleMetrics

      dynamicCounter <-
        createCounterVector
          ExampleCounterVector
          store
          Map.toList
      CounterVector.add dynamicCounter (HashMap.singleton "label_name_1" "label value 1") 10
      CounterVector.add dynamicCounter (HashMap.singleton "label_name_2" "label value 1") 10

      prometheusSample1 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample1
        "# HELP my_dynamic_counter Example counter with dynamic labels\n\
        \# TYPE my_dynamic_counter counter\n\
        \my_dynamic_counter{label_name_2=\"label value 1\"} 10.0\n\
        \my_dynamic_counter{label_name_1=\"label value 1\"} 10.0\n"

      CounterVector.add dynamicCounter (HashMap.singleton "label_name_2" "label value 1") 10
      CounterVector.add dynamicCounter (HashMap.singleton "label_name_2" "label value 2") 10
      CounterVector.add dynamicCounter (HashMap.singleton "label_name_3" "label value 1") 10

      prometheusSample2 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample2
        "# HELP my_dynamic_counter Example counter with dynamic labels\n\
        \# TYPE my_dynamic_counter counter\n\
        \my_dynamic_counter{label_name_2=\"label value 1\"} 20.0\n\
        \my_dynamic_counter{label_name_2=\"label value 2\"} 10.0\n\
        \my_dynamic_counter{label_name_3=\"label value 1\"} 10.0\n\
        \my_dynamic_counter{label_name_1=\"label value 1\"} 10.0\n"

    it "is adding new lables (for gauge) while sampling" $ do
      store <- newStore @ExampleMetrics

      dynamicGauge <-
        createGaugeVector
          ExampleGaugeVector
          store
          Map.toList
      GaugeVector.set dynamicGauge (HashMap.singleton "label_name_1" "label value 1") 10
      GaugeVector.set dynamicGauge (HashMap.singleton "label_name_2" "label value 1") 10

      prometheusSample1 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample1
        "# HELP my_dynamic_gauge Example gauge with dynamic labels\n\
        \# TYPE my_dynamic_gauge gauge\n\
        \my_dynamic_gauge{label_name_2=\"label value 1\"} 10.0\n\
        \my_dynamic_gauge{label_name_1=\"label value 1\"} 10.0\n"

      GaugeVector.set dynamicGauge (HashMap.singleton "label_name_2" "label value 1") 10
      GaugeVector.set dynamicGauge (HashMap.singleton "label_name_2" "label value 2") 10
      GaugeVector.set dynamicGauge (HashMap.singleton "label_name_3" "label value 1") 10

      prometheusSample2 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample2
        "# HELP my_dynamic_gauge Example gauge with dynamic labels\n\
        \# TYPE my_dynamic_gauge gauge\n\
        \my_dynamic_gauge{label_name_2=\"label value 1\"} 10.0\n\
        \my_dynamic_gauge{label_name_2=\"label value 2\"} 10.0\n\
        \my_dynamic_gauge{label_name_3=\"label value 1\"} 10.0\n\
        \my_dynamic_gauge{label_name_1=\"label value 1\"} 10.0\n"

    it "is adding new lables (for histogram) while sampling" $ do
      store <- newStore @ExampleMetrics

      let defaultBuckets = [1, 10, 100]
      dynamicHistogram <-
        createHistogramVector
          defaultBuckets
          ExampleHistogramVector
          store
          Map.toList
      HistogramVector.observe dynamicHistogram (HashMap.singleton "label_name_1" "label value 1") 10
      HistogramVector.observe dynamicHistogram (HashMap.singleton "label_name_2" "label value 1") 10

      prometheusSample1 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample1
        "# HELP my_dynamic_histogram Example histogram with dynamic labels\n\
        \# TYPE my_dynamic_histogram histogram\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"10.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"100.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"+Inf\"} 1\n\
        \my_dynamic_histogram_sum{label_name_2=\"label value 1\"} 10.0\n\
        \my_dynamic_histogram_count{label_name_2=\"label value 1\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"10.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"100.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"+Inf\"} 1\n\
        \my_dynamic_histogram_sum{label_name_1=\"label value 1\"} 10.0\n\
        \my_dynamic_histogram_count{label_name_1=\"label value 1\"} 1\n"

      HistogramVector.observe dynamicHistogram (HashMap.singleton "label_name_2" "label value 1") 100
      HistogramVector.observe dynamicHistogram (HashMap.singleton "label_name_2" "label value 2") 100
      HistogramVector.observe dynamicHistogram (HashMap.singleton "label_name_3" "label value 1") 100

      prometheusSample2 <-
        BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

      shouldBe
        prometheusSample2
        "# HELP my_dynamic_histogram Example histogram with dynamic labels\n\
        \# TYPE my_dynamic_histogram histogram\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"10.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"100.0\"} 2\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 1\",le=\"+Inf\"} 2\n\
        \my_dynamic_histogram_sum{label_name_2=\"label value 1\"} 110.0\n\
        \my_dynamic_histogram_count{label_name_2=\"label value 1\"} 2\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 2\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 2\",le=\"10.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 2\",le=\"100.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_2=\"label value 2\",le=\"+Inf\"} 1\n\
        \my_dynamic_histogram_sum{label_name_2=\"label value 2\"} 100.0\n\
        \my_dynamic_histogram_count{label_name_2=\"label value 2\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_3=\"label value 1\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_3=\"label value 1\",le=\"10.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_3=\"label value 1\",le=\"100.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_3=\"label value 1\",le=\"+Inf\"} 1\n\
        \my_dynamic_histogram_sum{label_name_3=\"label value 1\"} 100.0\n\
        \my_dynamic_histogram_count{label_name_3=\"label value 1\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"1.0\"} 0\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"10.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"100.0\"} 1\n\
        \my_dynamic_histogram_bucket{label_name_1=\"label value 1\",le=\"+Inf\"} 1\n\
        \my_dynamic_histogram_sum{label_name_1=\"label value 1\"} 10.0\n\
        \my_dynamic_histogram_count{label_name_1=\"label value 1\"} 1\n"

data ExampleMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
  ExampleCounterVector ::
    ExampleMetrics
      "my_dynamic_counter"
      "Example counter with dynamic labels"
      'CounterType
      (HashMap.HashMap T.Text T.Text)
  ExampleGaugeVector ::
    ExampleMetrics
      "my_dynamic_gauge"
      "Example gauge with dynamic labels"
      'GaugeType
      (HashMap.HashMap T.Text T.Text)
  ExampleHistogramVector ::
    ExampleMetrics
      "my_dynamic_histogram"
      "Example histogram with dynamic labels"
      'HistogramType
      (HashMap.HashMap T.Text T.Text)
