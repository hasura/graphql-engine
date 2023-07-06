{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Example
  ( main,
  )
where

import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.Counter as Counter
import qualified System.Metrics.Prometheus.Gauge as Gauge

-- Custom type describing a set of classes of metrics.
data
  MyMetrics ::
    Symbol -> -- Name
    Symbol -> -- Help
    MetricType ->
    Type -> -- Labels
    Type
  where
  Requests ::
    MyMetrics "requests" "" 'CounterType EndpointLabels
  DBConnections ::
    MyMetrics "postgres.total_connections" "" 'GaugeType DataSourceLabels

-- Custom label set
newtype EndpointLabels = EndpointLabels {endpoint :: T.Text}
  deriving (Generic)

instance ToLabels EndpointLabels

-- Custom label set
data DataSourceLabels = DataSourceLabels
  { sourceName :: T.Text,
    connInfo :: T.Text
  }
  deriving (Generic)

instance ToLabels DataSourceLabels

main :: IO ()
main = do
  store <- newStore
  harpsichordReqs <-
    createCounter Requests (EndpointLabels "dev/harpsichord") store
  tablaReqs <-
    createCounter Requests (EndpointLabels "dev/tabla") store
  dbConnections <-
    let labels =
          DataSourceLabels
            { sourceName = "myDB",
              connInfo = "localhost:5432"
            }
     in createGauge DBConnections labels store

  Counter.add harpsichordReqs 5
  Counter.add tablaReqs 10
  Gauge.set dbConnections 99

  stats <- sampleAll store
  print stats
