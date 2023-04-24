-- | Mutable references for Prometheus metrics.
--
-- These metrics are independent from the metrics in "Hasura.Server.Metrics".
module Hasura.Server.Prometheus
  ( PrometheusMetrics (..),
    GraphQLRequestMetrics (..),
    EventTriggerMetrics (..),
    makeDummyPrometheusMetrics,
    ConnectionsGauge,
    Connections (..),
    newConnectionsGauge,
    readConnectionsGauge,
    incWarpThreads,
    decWarpThreads,
    incWebsocketConnections,
    decWebsocketConnections,
    ScheduledTriggerMetrics (..),
    SubscriptionMetrics (..),
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Hasura.Prelude
import System.Metrics.Prometheus.Counter (Counter)
import System.Metrics.Prometheus.Counter qualified as Counter
import System.Metrics.Prometheus.Gauge (Gauge)
import System.Metrics.Prometheus.Gauge qualified as Gauge
import System.Metrics.Prometheus.Histogram (Histogram)
import System.Metrics.Prometheus.Histogram qualified as Histogram

--------------------------------------------------------------------------------

-- | Mutable references for Prometheus metrics.
data PrometheusMetrics = PrometheusMetrics
  { pmConnections :: ConnectionsGauge,
    pmActiveSubscriptions :: Gauge,
    pmGraphQLRequestMetrics :: GraphQLRequestMetrics,
    pmEventTriggerMetrics :: EventTriggerMetrics,
    pmWebSocketBytesReceived :: Counter,
    pmWebSocketBytesSent :: Counter,
    pmActionBytesReceived :: Counter,
    pmActionBytesSent :: Counter,
    pmScheduledTriggerMetrics :: ScheduledTriggerMetrics,
    pmSubscriptionMetrics :: SubscriptionMetrics
  }

data GraphQLRequestMetrics = GraphQLRequestMetrics
  { gqlRequestsQuerySuccess :: Counter,
    gqlRequestsQueryFailure :: Counter,
    gqlRequestsMutationSuccess :: Counter,
    gqlRequestsMutationFailure :: Counter,
    gqlRequestsUnknownFailure :: Counter,
    gqlExecutionTimeSecondsQuery :: Histogram,
    gqlExecutionTimeSecondsMutation :: Histogram
  }

data EventTriggerMetrics = EventTriggerMetrics
  { eventTriggerHTTPWorkers :: Gauge,
    eventQueueTimeSeconds :: Histogram,
    eventsFetchTimePerBatch :: Histogram,
    eventWebhookProcessingTime :: Histogram,
    eventProcessingTime :: Histogram,
    eventTriggerBytesReceived :: Counter,
    eventTriggerBytesSent :: Counter,
    eventProcessedTotalSuccess :: Counter,
    eventProcessedTotalFailure :: Counter,
    eventInvocationTotalSuccess :: Counter,
    eventInvocationTotalFailure :: Counter
  }

data ScheduledTriggerMetrics = ScheduledTriggerMetrics
  { stmScheduledTriggerBytesReceived :: Counter,
    stmScheduledTriggerBytesSent :: Counter,
    stmCronEventsInvocationTotalSuccess :: Counter,
    stmCronEventsInvocationTotalFailure :: Counter,
    stmOneOffEventsInvocationTotalSuccess :: Counter,
    stmOneOffEventsInvocationTotalFailure :: Counter,
    stmCronEventsProcessedTotalSuccess :: Counter,
    stmCronEventsProcessedTotalFailure :: Counter,
    stmOneOffEventsProcessedTotalSuccess :: Counter,
    stmOneOffEventsProcessedTotalFailure :: Counter
  }

data SubscriptionMetrics = SubscriptionMetrics
  { submActiveLiveQueryPollers :: Gauge,
    submActiveStreamingPollers :: Gauge,
    submActiveLiveQueryPollersInError :: Gauge,
    submActiveStreamingPollersInError :: Gauge
  }

-- | Create dummy mutable references without associating them to a metrics
-- store.
makeDummyPrometheusMetrics :: IO PrometheusMetrics
makeDummyPrometheusMetrics = do
  pmConnections <- newConnectionsGauge
  pmActiveSubscriptions <- Gauge.new
  pmGraphQLRequestMetrics <- makeDummyGraphQLRequestMetrics
  pmEventTriggerMetrics <- makeDummyEventTriggerMetrics
  pmWebSocketBytesReceived <- Counter.new
  pmWebSocketBytesSent <- Counter.new
  pmActionBytesReceived <- Counter.new
  pmActionBytesSent <- Counter.new
  pmScheduledTriggerMetrics <- makeDummyScheduledTriggerMetrics
  pmSubscriptionMetrics <- makeDummySubscriptionMetrics
  pure PrometheusMetrics {..}

makeDummyGraphQLRequestMetrics :: IO GraphQLRequestMetrics
makeDummyGraphQLRequestMetrics = do
  gqlRequestsQuerySuccess <- Counter.new
  gqlRequestsQueryFailure <- Counter.new
  gqlRequestsMutationSuccess <- Counter.new
  gqlRequestsMutationFailure <- Counter.new
  gqlRequestsUnknownFailure <- Counter.new
  gqlExecutionTimeSecondsQuery <- Histogram.new []
  gqlExecutionTimeSecondsMutation <- Histogram.new []
  pure GraphQLRequestMetrics {..}

makeDummyEventTriggerMetrics :: IO EventTriggerMetrics
makeDummyEventTriggerMetrics = do
  eventTriggerHTTPWorkers <- Gauge.new
  eventQueueTimeSeconds <- Histogram.new []
  eventsFetchTimePerBatch <- Histogram.new []
  eventWebhookProcessingTime <- Histogram.new []
  eventProcessingTime <- Histogram.new []
  eventTriggerBytesReceived <- Counter.new
  eventTriggerBytesSent <- Counter.new
  eventProcessedTotalSuccess <- Counter.new
  eventProcessedTotalFailure <- Counter.new
  eventInvocationTotalSuccess <- Counter.new
  eventInvocationTotalFailure <- Counter.new
  pure EventTriggerMetrics {..}

makeDummyScheduledTriggerMetrics :: IO ScheduledTriggerMetrics
makeDummyScheduledTriggerMetrics = do
  stmScheduledTriggerBytesReceived <- Counter.new
  stmScheduledTriggerBytesSent <- Counter.new
  stmCronEventsInvocationTotalSuccess <- Counter.new
  stmCronEventsInvocationTotalFailure <- Counter.new
  stmOneOffEventsInvocationTotalSuccess <- Counter.new
  stmOneOffEventsInvocationTotalFailure <- Counter.new
  stmCronEventsProcessedTotalSuccess <- Counter.new
  stmCronEventsProcessedTotalFailure <- Counter.new
  stmOneOffEventsProcessedTotalSuccess <- Counter.new
  stmOneOffEventsProcessedTotalFailure <- Counter.new
  pure ScheduledTriggerMetrics {..}

makeDummySubscriptionMetrics :: IO SubscriptionMetrics
makeDummySubscriptionMetrics = do
  submActiveLiveQueryPollers <- Gauge.new
  submActiveStreamingPollers <- Gauge.new
  submActiveLiveQueryPollersInError <- Gauge.new
  submActiveStreamingPollersInError <- Gauge.new
  pure SubscriptionMetrics {..}

--------------------------------------------------------------------------------

-- | A mutable reference for atomically sampling the number of websocket
-- connections and number of threads forked by the warp webserver.
--
-- Because we derive the number of (non-websocket) HTTP connections by the
-- difference of these two metrics, we must sample them simultaneously,
-- otherwise we might report a negative number of HTTP connections.
newtype ConnectionsGauge = ConnectionsGauge (IORef Connections)

data Connections = Connections
  { connWarpThreads :: Int64,
    connWebsockets :: Int64
  }

newConnectionsGauge :: IO ConnectionsGauge
newConnectionsGauge =
  ConnectionsGauge
    <$> newIORef Connections {connWarpThreads = 0, connWebsockets = 0}

readConnectionsGauge :: ConnectionsGauge -> IO Connections
readConnectionsGauge (ConnectionsGauge ref) = readIORef ref

incWarpThreads :: ConnectionsGauge -> IO ()
incWarpThreads =
  modifyConnectionsGauge $ \connections ->
    connections {connWarpThreads = connWarpThreads connections + 1}

decWarpThreads :: ConnectionsGauge -> IO ()
decWarpThreads =
  modifyConnectionsGauge $ \connections ->
    connections {connWarpThreads = connWarpThreads connections - 1}

incWebsocketConnections :: ConnectionsGauge -> IO ()
incWebsocketConnections =
  modifyConnectionsGauge $ \connections ->
    connections {connWebsockets = connWebsockets connections + 1}

decWebsocketConnections :: ConnectionsGauge -> IO ()
decWebsocketConnections =
  modifyConnectionsGauge $ \connections ->
    connections {connWebsockets = connWebsockets connections - 1}

modifyConnectionsGauge ::
  (Connections -> Connections) -> ConnectionsGauge -> IO ()
modifyConnectionsGauge f (ConnectionsGauge ref) =
  atomicModifyIORef' ref $ \connections -> (f connections, ())
