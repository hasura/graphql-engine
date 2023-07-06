-- | This module defines a type for mutable map of lables and `Counter`. `CounterVector` can be used where the labels of
-- the derived metrics are not known statically during registering. New labels can be added during the runtime using
-- `add` or `inc` methods.
module System.Metrics.Prometheus.CounterVector
  ( CounterVector,
    new,
    add,
    read,
    readInt,
    inc,
  )
where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import System.Metrics.Prometheus.Counter (Counter)
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.MetricVector (MetricVector)
import qualified System.Metrics.Prometheus.MetricVector as MetricVector
import Prelude hiding (read)

-- | A mutable map of labels and `Counter`
type CounterVector label = MetricVector label Counter

-- | Create a new empty `CounterVector`
new :: (Ord label) => IO (CounterVector label)
new = MetricVector.new

-- | Increment the `Counter` for the given label by the given amount. If the label is not present in the
-- `CounterVector`, a new `Counter` is created.
add :: (Ord label) => CounterVector label -> label -> Int64 -> IO ()
add counterMetrics labelSet increment = do
  counter <- MetricVector.getMetric Counter.new counterMetrics labelSet
  Counter.add counter increment

-- | Read the current value of all `Counter`s in the `CounterVector`
read :: CounterVector label -> IO (Map label Double)
read counterMetrics = do
  counterMap <- MetricVector.getMetrics counterMetrics
  traverse Counter.read counterMap

-- | Read the current value of all `Counter`s in the `CounterVector` as `Int64`
readInt :: CounterVector label -> IO (Map label Int64)
readInt counterMetrics = do
  counterMap <- MetricVector.getMetrics counterMetrics
  traverse Counter.readInt counterMap

-- | Increment the `Counter` for the given label by 1. If the label is not present in the `CounterVector`, a
-- new `Counter` is created.
inc :: (Ord label) => CounterVector label -> label -> IO ()
inc counterVector labelSet = add counterVector labelSet 1
