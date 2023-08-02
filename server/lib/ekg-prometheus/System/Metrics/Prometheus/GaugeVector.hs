-- | This module defines a type for mutable map of lables and `Gauge`. `GaugeVector` can be used where the labels of
-- the derived metrics are not known statically during registering.
module System.Metrics.Prometheus.GaugeVector
  ( GaugeVector,
    new,
    read,
    readInt,
    inc,
    dec,
    add,
    subtract,
    set,
  )
where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import System.Metrics.Prometheus.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Gauge as Gauge
import System.Metrics.Prometheus.MetricVector (MetricVector)
import qualified System.Metrics.Prometheus.MetricVector as MetricVector
import Prelude hiding (read, subtract)

-- | A mutable map of labels and `Gauge`
type GaugeVector label = MetricVector label Gauge

-- | Create a new empty `GaugeVector`
new :: (Ord label) => IO (GaugeVector label)
new = MetricVector.new

-- | Read the current value of all `Gauge`s in the `GaugeVector`
read :: GaugeVector label -> IO (Map label Double)
read gaugeMetrics = do
  gaugeMap <- MetricVector.getMetrics gaugeMetrics
  traverse Gauge.read gaugeMap

-- | Read the current value of all `Gauge`s in the `GaugeVector` as `Int64`
readInt :: GaugeVector label -> IO (Map label Int64)
readInt gaugeMetrics = do
  gaugeMap <- MetricVector.getMetrics gaugeMetrics
  traverse Gauge.readInt gaugeMap

-- | Increment the `Gauge` for the given label by 1. If the label is not present in the `GaugeVector`, a
-- new `Gauge` is created.
inc :: (Ord label) => GaugeVector label -> label -> IO ()
inc gaugeVector labelSet = add gaugeVector labelSet 1

-- | Decrement the `Gauge` for the given label by 1. If the label is not present in the `GaugeVector`, a
-- new `Gauge` is created.
dec :: (Ord label) => GaugeVector label -> label -> IO ()
dec gaugeVector labelSet = subtract gaugeVector labelSet 1

-- | Increase the `Gauge` for the given label by the given amount. If the label is not present in the
-- `GaugeVector`, a new `Gauge` is created.
add :: (Ord label) => GaugeVector label -> label -> Int64 -> IO ()
add gaugeMetrics labelSet increment = do
  gauge <- MetricVector.getMetric Gauge.new gaugeMetrics labelSet
  Gauge.add gauge increment

-- | Decrease the `Gauge` for the given label by the given amount. If the label is not present in the
-- `GaugeVector`, a new `Gauge` is created.
subtract :: (Ord label) => GaugeVector label -> label -> Int64 -> IO ()
subtract gaugeMetrics labelSet decrement = do
  gauge <- MetricVector.getMetric Gauge.new gaugeMetrics labelSet
  Gauge.subtract gauge decrement

-- | Set the `Gauge` for the given label to the given value. If the label is not present in the
-- `GaugeVector`, a new `Gauge` is created.
set :: (Ord label) => GaugeVector label -> label -> Int64 -> IO ()
set gaugeMetrics labelSet value = do
  gauge <- MetricVector.getMetric Gauge.new gaugeMetrics labelSet
  Gauge.set gauge value
