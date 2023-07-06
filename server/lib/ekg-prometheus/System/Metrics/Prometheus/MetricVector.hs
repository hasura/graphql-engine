{-# LANGUAGE LambdaCase #-}

module System.Metrics.Prometheus.MetricVector
  ( MetricVector,
    new,
    getMetric,
    getMetrics,
  )
where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (read)

newtype MetricVector label metric = C (IORef (Map label metric))

new :: (Ord label) => IO (MetricVector label metric)
new = C <$> newIORef mempty

getMetric :: (Ord label) => IO metric -> MetricVector label metric -> label -> IO metric
getMetric newMetric (C ref) label = do
  readIORef ref
    >>= ( \case
            Just metric -> pure metric
            Nothing -> do
              metric <- newMetric
              atomicModifyIORef' ref $ \metricMap ->
                case Map.lookup label metricMap of
                  Just existingMetric -> (metricMap, existingMetric)
                  Nothing ->
                    let newMetricMap = Map.insert label metric metricMap
                     in (newMetricMap, metric)
        )
      . Map.lookup label

getMetrics :: MetricVector label metric -> IO (Map label metric)
getMetrics (C ref) = readIORef ref
