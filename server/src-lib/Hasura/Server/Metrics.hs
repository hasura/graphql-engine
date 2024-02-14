module Hasura.Server.Metrics
  ( ServerMetricsSpec (..),
    ServerMetrics (..),
    createServerMetrics,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Hasura.Prelude
import System.Metrics
import System.Metrics.Distribution (Distribution)
import System.Metrics.Gauge (Gauge)

-- | A specification of the metrics tracked by the server.
--
-- The use of the "unit" type () for the "tag structure" type parameter of a
-- metric indicates that we prohibit that metric from being annotated with
-- tags.
data
  ServerMetricsSpec ::
    Symbol -> -- Metric name
    MetricType -> -- Metric type, e.g. Counter, Gauge
    Type -> -- Tag structure
    Type
  where
  -- | Current Number of active Warp threads
  WarpThreads ::
    ServerMetricsSpec
      "warp_threads"
      'GaugeType
      ()
  -- | Current number of active websocket connections
  WebsocketConnections ::
    ServerMetricsSpec
      "websocket_connections"
      'GaugeType
      ()
  -- | Current number of active subscriptions
  ActiveSubscriptions ::
    ServerMetricsSpec
      "active_subscriptions"
      'GaugeType
      ()
  -- | Total Number of events fetched from last 'Event Trigger Fetch'
  NumEventsFetchedPerBatch ::
    ServerMetricsSpec
      "events_fetched_per_batch"
      'DistributionType
      ()
  -- | Current number of Event trigger's HTTP workers in process
  NumEventHTTPWorkers ::
    ServerMetricsSpec
      "num_event_trigger_http_workers"
      'GaugeType
      ()
  -- | Time (in seconds) between the 'Event Trigger Fetch' from DB and the
  -- processing of the event
  EventQueueTime ::
    ServerMetricsSpec
      "event_queue_time"
      'DistributionType
      ()
  -- | The current schema cache metadata resource version
  SchemaCacheMetadataResourceVersion ::
    ServerMetricsSpec
      "schema_cache_metadata_resource_version"
      'GaugeType
      ()
  -- | Current number of subscribers connected to live queries
  ActiveLiveQueries ::
    ServerMetricsSpec
      "active_livequeries"
      'GaugeType
      ()
  -- | Current number of subscribers connected to streaming subscriptions
  ActiveStreaming ::
    ServerMetricsSpec
      "active_streaming_subscriptions"
      'GaugeType
      ()
  -- | Latency of fetching a batch of events
  EventFetchTimePerBatch ::
    ServerMetricsSpec
      "events_fetch_time_per_batch"
      'DistributionType
      ()
  -- | The time (in seconds) between when a event is picked for delivery to the
  --   time its status is updated in the DB
  EventWebhookProcessingTime ::
    ServerMetricsSpec
      "event_webhook_processing_time"
      'DistributionType
      ()
  -- | The time taken for an event to be delivered since it's been created (if
  -- first attempt) or retried (after first attempt)
  EventProcessingTime ::
    ServerMetricsSpec
      "event_processing_time"
      'DistributionType
      ()

-- | Mutable references for the server metrics. See `ServerMetricsSpec` for a
-- description of each metric.
data ServerMetrics = ServerMetrics
  { smWarpThreads :: Gauge,
    smWebsocketConnections :: Gauge,
    smActiveSubscriptions :: Gauge,
    smNumEventsFetchedPerBatch :: Distribution,
    smNumEventHTTPWorkers :: Gauge,
    smEventQueueTime :: Distribution,
    smSchemaCacheMetadataResourceVersion :: Gauge,
    smActiveLiveQueries :: Gauge,
    smActiveStreamingSubscriptions :: Gauge,
    smEventFetchTimePerBatch :: Distribution,
    smEventWebhookProcessingTime :: Distribution,
    smEventProcessingTime :: Distribution
  }

createServerMetrics :: Store ServerMetricsSpec -> IO ServerMetrics
createServerMetrics store = do
  smWarpThreads <- createGauge WarpThreads () store
  smWebsocketConnections <- createGauge WebsocketConnections () store
  smActiveSubscriptions <- createGauge ActiveSubscriptions () store
  smNumEventsFetchedPerBatch <- createDistribution NumEventsFetchedPerBatch () store
  smNumEventHTTPWorkers <- createGauge NumEventHTTPWorkers () store
  smEventQueueTime <- createDistribution EventQueueTime () store
  smSchemaCacheMetadataResourceVersion <- createGauge SchemaCacheMetadataResourceVersion () store
  smActiveLiveQueries <- createGauge ActiveLiveQueries () store
  smActiveStreamingSubscriptions <- createGauge ActiveStreaming () store
  smEventFetchTimePerBatch <- createDistribution EventFetchTimePerBatch () store
  smEventWebhookProcessingTime <- createDistribution EventWebhookProcessingTime () store
  smEventProcessingTime <- createDistribution EventProcessingTime () store
  pure ServerMetrics {..}
