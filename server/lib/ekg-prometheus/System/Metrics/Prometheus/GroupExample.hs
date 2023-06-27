{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module System.Metrics.Prometheus.GroupExample
  ( main,
  )
where

import Data.Kind (Type)
import GHC.Stats
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus

data
  RTSMetrics ::
    Symbol -> -- Name
    Symbol -> -- Help
    MetricType ->
    Type -> -- Labels
    Type
  where
  RTSGcs :: RTSMetrics "gcs" "" 'CounterType ()
  RTSMaxLiveBytes :: RTSMetrics "max_live_bytes" "" 'GaugeType ()

main :: IO ()
main = do
  store <- newStore
  let samplingGroup =
        SamplingGroup
          :> (RTSGcs, (), fromIntegral . gcs)
          :> (RTSMaxLiveBytes, (), fromIntegral . max_live_bytes)
  registerPermanently store $ registerGroup samplingGroup getRTSStats
