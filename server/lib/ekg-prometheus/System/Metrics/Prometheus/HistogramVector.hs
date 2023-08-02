-- | This module defines a type for mutable map of lables and `Histogram`. `HistogramVector` can be used where the labels of
-- the derived metrics are not known statically during registering. New labels can be added during the runtime using `observe`.
module System.Metrics.Prometheus.HistogramVector
  ( HistogramVector,
    new,
    read,
    observe,
  )
where

import Data.Map.Strict (Map)
import System.Metrics.Prometheus.Histogram (Histogram, HistogramSample, UpperBound)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import System.Metrics.Prometheus.MetricVector (MetricVector)
import qualified System.Metrics.Prometheus.MetricVector as MetricVector
import Prelude hiding (read, subtract)

-- | A mutable map of labels and `Histogram`
data HistogramVector label = HistogramVector [UpperBound] (MetricVector label Histogram)

-- | Create a new empty `HistogramVector`
new :: (Ord label) => [UpperBound] -> IO (HistogramVector label)
new buckets = HistogramVector buckets <$> MetricVector.new

-- | Read the current value of all `Histogram`s in the `HistogramVector`
read :: HistogramVector label -> IO (Map label HistogramSample)
read (HistogramVector _ histogramMetrics) = do
  histogramMap <- MetricVector.getMetrics histogramMetrics
  traverse Histogram.read histogramMap

-- | Observe the given value for the given label. If the label is not present in the `HistogramVector`, a new
-- `Histogram` is created.
observe :: (Ord label) => HistogramVector label -> label -> Double -> IO ()
observe (HistogramVector buckets histogramMetrics) labelSet increment = do
  histogram <- MetricVector.getMetric (Histogram.new buckets) histogramMetrics labelSet
  Histogram.observe histogram increment
