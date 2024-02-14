{-# LANGUAGE DeriveAnyClass #-}

-- | Mutable references for Prometheus metrics.
--
-- These metrics are independent from the metrics in "Hasura.Server.Metrics".
module Hasura.Server.Prometheus
  ( PrometheusMetrics (..),
    GraphQLRequestMetrics (..),
    EventTriggerMetrics (..),
    CacheRequestMetrics (..),
    OpenTelemetryMetrics (..),
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
    DynamicEventTriggerLabel (..),
    ResponseStatus (..),
    responseStatusToLabelValue,
    EventStatusLabel (..),
    eventSuccessLabel,
    eventFailedLabel,
    EventStatusWithTriggerLabel (..),
    GranularPrometheusMetricsState (..),
    observeHistogramWithLabel,
    SubscriptionKindLabel (..),
    SubscriptionLabel (..),
    DynamicSubscriptionLabel (..),
    streamingSubscriptionLabel,
    liveQuerySubscriptionLabel,
    recordMetricWithLabel,
    recordSubcriptionMetric,
  )
where

import Data.HashMap.Internal.Strict qualified as Map
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Transport.HTTP.Protocol (OperationName (..))
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName, sourceNameToText)
import Hasura.RQL.Types.EventTrigger (TriggerName, triggerNameToTxt)
import Hasura.Server.Types (GranularPrometheusMetricsState (..))
import Language.GraphQL.Draft.Syntax qualified as G
import System.Metrics.Prometheus (ToLabels (..))
import System.Metrics.Prometheus.Counter (Counter)
import System.Metrics.Prometheus.Counter qualified as Counter
import System.Metrics.Prometheus.CounterVector (CounterVector)
import System.Metrics.Prometheus.CounterVector qualified as CounterVector
import System.Metrics.Prometheus.Gauge (Gauge)
import System.Metrics.Prometheus.Gauge qualified as Gauge
import System.Metrics.Prometheus.GaugeVector qualified as GaugeVector
import System.Metrics.Prometheus.Histogram (Histogram)
import System.Metrics.Prometheus.Histogram qualified as Histogram
import System.Metrics.Prometheus.HistogramVector (HistogramVector)
import System.Metrics.Prometheus.HistogramVector qualified as HistogramVector

--------------------------------------------------------------------------------

-- | Mutable references for Prometheus metrics.
data PrometheusMetrics = PrometheusMetrics
  { pmConnections :: ConnectionsGauge,
    pmGraphQLRequestMetrics :: GraphQLRequestMetrics,
    pmEventTriggerMetrics :: EventTriggerMetrics,
    pmWebSocketBytesReceived :: Counter,
    pmWebSocketBytesSent :: CounterVector DynamicSubscriptionLabel,
    pmActionBytesReceived :: Counter,
    pmActionBytesSent :: Counter,
    pmScheduledTriggerMetrics :: ScheduledTriggerMetrics,
    pmSubscriptionMetrics :: SubscriptionMetrics,
    pmWebsocketMsgQueueTimeSeconds :: Histogram,
    pmWebsocketMsgWriteTimeSeconds :: Histogram,
    pmCacheRequestMetrics :: CacheRequestMetrics,
    pmOpenTelemetryMetrics :: OpenTelemetryMetrics
  }

data GraphQLRequestMetrics = GraphQLRequestMetrics
  { gqlRequestsQuerySuccess :: Counter,
    gqlRequestsQueryFailure :: Counter,
    gqlRequestsMutationSuccess :: Counter,
    gqlRequestsMutationFailure :: Counter,
    gqlRequestsSubscriptionSuccess :: Counter,
    gqlRequestsSubscriptionFailure :: Counter,
    gqlRequestsUnknownFailure :: Counter,
    gqlExecutionTimeSecondsQuery :: Histogram,
    gqlExecutionTimeSecondsMutation :: Histogram
  }

data EventTriggerMetrics = EventTriggerMetrics
  { eventTriggerHTTPWorkers :: Gauge,
    eventsFetchedPerBatch :: Gauge,
    eventQueueTimeSeconds :: HistogramVector (Maybe DynamicEventTriggerLabel),
    eventsFetchTimePerBatch :: Histogram,
    eventWebhookProcessingTime :: HistogramVector (Maybe DynamicEventTriggerLabel),
    eventProcessingTime :: HistogramVector (Maybe DynamicEventTriggerLabel),
    eventTriggerBytesReceived :: Counter,
    eventTriggerBytesSent :: Counter,
    eventProcessedTotal :: CounterVector EventStatusWithTriggerLabel,
    eventInvocationTotal :: CounterVector EventStatusWithTriggerLabel
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
    submActiveStreamingPollersInError :: Gauge,
    submTotalTime :: HistogramVector.HistogramVector SubscriptionLabel,
    submDBExecTotalTime :: HistogramVector.HistogramVector SubscriptionLabel,
    submActiveSubscriptions :: GaugeVector.GaugeVector SubscriptionLabel
  }

data CacheRequestMetrics = CacheRequestMetrics
  { crmCacheHits :: Counter,
    crmCacheMisses :: Counter
  }

-- | Metrics related to OTel telemetry export; for now the volume of logs and
-- trace spans shipped, and counts of log lines and spans dropped due to high
-- volume.
data OpenTelemetryMetrics = OpenTelemetryMetrics
  { otmSentSpans :: Counter,
    -- | Dropped due to the send buffer being full
    otmDroppedSpansInBuffer :: Counter,
    -- | Dropped due to some error (after retrying) when sending to collector
    otmDroppedSpansInSend :: Counter,
    otmSentLogs :: Counter,
    otmDroppedLogsInBuffer :: Counter,
    otmDroppedLogsInSend :: Counter
  }

-- | Create dummy mutable references without associating them to a metrics
-- store.
makeDummyPrometheusMetrics :: IO PrometheusMetrics
makeDummyPrometheusMetrics = do
  pmConnections <- newConnectionsGauge
  pmGraphQLRequestMetrics <- makeDummyGraphQLRequestMetrics
  pmEventTriggerMetrics <- makeDummyEventTriggerMetrics
  pmWebSocketBytesReceived <- Counter.new
  pmWebSocketBytesSent <- CounterVector.new
  pmActionBytesReceived <- Counter.new
  pmActionBytesSent <- Counter.new
  pmScheduledTriggerMetrics <- makeDummyScheduledTriggerMetrics
  pmSubscriptionMetrics <- makeDummySubscriptionMetrics
  pmWebsocketMsgQueueTimeSeconds <- Histogram.new []
  pmWebsocketMsgWriteTimeSeconds <- Histogram.new []
  pmCacheRequestMetrics <- makeDummyCacheRequestMetrics
  pmOpenTelemetryMetrics <- makeDummyOpenTelemetryMetrics
  pure PrometheusMetrics {..}

makeDummyGraphQLRequestMetrics :: IO GraphQLRequestMetrics
makeDummyGraphQLRequestMetrics = do
  gqlRequestsQuerySuccess <- Counter.new
  gqlRequestsQueryFailure <- Counter.new
  gqlRequestsMutationSuccess <- Counter.new
  gqlRequestsMutationFailure <- Counter.new
  gqlRequestsSubscriptionSuccess <- Counter.new
  gqlRequestsSubscriptionFailure <- Counter.new
  gqlRequestsUnknownFailure <- Counter.new
  gqlExecutionTimeSecondsQuery <- Histogram.new []
  gqlExecutionTimeSecondsMutation <- Histogram.new []
  pure GraphQLRequestMetrics {..}

makeDummyEventTriggerMetrics :: IO EventTriggerMetrics
makeDummyEventTriggerMetrics = do
  eventTriggerHTTPWorkers <- Gauge.new
  eventsFetchedPerBatch <- Gauge.new
  eventQueueTimeSeconds <- HistogramVector.new []
  eventsFetchTimePerBatch <- Histogram.new []
  eventWebhookProcessingTime <- HistogramVector.new []
  eventProcessingTime <- HistogramVector.new []
  eventTriggerBytesReceived <- Counter.new
  eventTriggerBytesSent <- Counter.new
  eventProcessedTotal <- CounterVector.new
  eventInvocationTotal <- CounterVector.new
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
  submTotalTime <- HistogramVector.new []
  submDBExecTotalTime <- HistogramVector.new []
  submActiveSubscriptions <- GaugeVector.new
  pure SubscriptionMetrics {..}

makeDummyCacheRequestMetrics :: IO CacheRequestMetrics
makeDummyCacheRequestMetrics = do
  crmCacheHits <- Counter.new
  crmCacheMisses <- Counter.new
  pure CacheRequestMetrics {..}

makeDummyOpenTelemetryMetrics :: IO OpenTelemetryMetrics
makeDummyOpenTelemetryMetrics = do
  otmSentSpans <- Counter.new
  otmDroppedSpansInSend <- Counter.new
  otmDroppedSpansInBuffer <- Counter.new
  otmSentLogs <- Counter.new
  otmDroppedLogsInSend <- Counter.new
  otmDroppedLogsInBuffer <- Counter.new
  pure OpenTelemetryMetrics {..}

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

data DynamicEventTriggerLabel = DynamicEventTriggerLabel
  { _detlTriggerName :: TriggerName,
    _detlSourceName :: SourceName
  }
  deriving (Ord, Eq)

instance ToLabels (Maybe DynamicEventTriggerLabel) where
  toLabels Nothing = Map.empty
  toLabels (Just (DynamicEventTriggerLabel triggerName sourceName)) = Map.fromList $ [("trigger_name", triggerNameToTxt triggerName), ("source_name", sourceNameToText sourceName)]

data ResponseStatus = Success | Failed

-- TODO: Make this a method of a new typeclass of the metrics library
responseStatusToLabelValue :: ResponseStatus -> Text
responseStatusToLabelValue = \case
  Success -> "success"
  Failed -> "failed"

newtype EventStatusLabel = EventStatusLabel
  { status :: Text
  }
  deriving stock (Generic, Ord, Eq)
  deriving anyclass (ToLabels)

eventSuccessLabel :: EventStatusLabel
eventSuccessLabel = EventStatusLabel $ responseStatusToLabelValue Success

eventFailedLabel :: EventStatusLabel
eventFailedLabel = EventStatusLabel $ responseStatusToLabelValue Failed

data EventStatusWithTriggerLabel = EventStatusWithTriggerLabel
  { _eswtlStatus :: EventStatusLabel,
    _eswtlDynamicLabels :: Maybe DynamicEventTriggerLabel
  }
  deriving stock (Generic, Ord, Eq)

instance ToLabels (EventStatusWithTriggerLabel) where
  toLabels (EventStatusWithTriggerLabel esl tl) = (HashMap.fromList $ [("status", status esl)]) <> toLabels tl

data SubscriptionKindLabel = SubscriptionKindLabel
  { subscription_kind :: Text
  }
  deriving stock (Generic, Ord, Eq)
  deriving anyclass (ToLabels)

streamingSubscriptionLabel :: SubscriptionKindLabel
streamingSubscriptionLabel = SubscriptionKindLabel "streaming"

liveQuerySubscriptionLabel :: SubscriptionKindLabel
liveQuerySubscriptionLabel = SubscriptionKindLabel "live-query"

data DynamicSubscriptionLabel = DynamicSubscriptionLabel
  { _dslParamQueryHash :: Maybe ParameterizedQueryHash,
    _dslOperationName :: Maybe OperationName
  }
  deriving stock (Generic, Ord, Eq)

instance ToLabels DynamicSubscriptionLabel where
  toLabels (DynamicSubscriptionLabel hash opName) =
    Map.fromList
      $ maybe [] (\pqh -> [("parameterized_query_hash", bsToTxt $ unParamQueryHash pqh)]) hash
      <> maybe [] (\op -> [("operation_name", G.unName $ _unOperationName op)]) opName

data SubscriptionLabel = SubscriptionLabel
  { _slKind :: SubscriptionKindLabel,
    _slDynamicLabels :: Maybe DynamicSubscriptionLabel
  }
  deriving stock (Generic, Ord, Eq)

instance ToLabels SubscriptionLabel where
  toLabels (SubscriptionLabel kind Nothing) = Map.fromList $ [("subscription_kind", subscription_kind kind)]
  toLabels (SubscriptionLabel kind (Just dl)) = (Map.fromList $ [("subscription_kind", subscription_kind kind)]) <> toLabels dl

-- | Record metrics with dynamic label
recordMetricWithLabel ::
  (MonadIO m) =>
  (IO GranularPrometheusMetricsState) ->
  -- should the metric be observed without a label when granularMetricsState is OFF
  Bool ->
  -- the action to perform when granularMetricsState is ON
  IO () ->
  -- the action to perform when granularMetricsState is OFF
  IO () ->
  m ()
recordMetricWithLabel getMetricState alwaysObserve metricActionWithLabel metricActionWithoutLabel = do
  metricState <- liftIO $ getMetricState
  case metricState of
    GranularMetricsOn -> liftIO $ metricActionWithLabel
    -- Some metrics do not make sense without a dynamic label, hence only record the
    -- metric when alwaysObserve is set to true else do not record the metric
    GranularMetricsOff -> do
      when alwaysObserve
        $ liftIO
        $ metricActionWithoutLabel

-- | Observe a histogram metric with a label.
--
-- If the granularity is set to 'GranularMetricsOn', the label will be
-- included in the metric. Otherwise, the label will be set to `Nothing`
observeHistogramWithLabel ::
  (Ord l, MonadIO m) =>
  (IO GranularPrometheusMetricsState) ->
  -- should the metric be observed without a label when granularMetricsState is OFF
  Bool ->
  HistogramVector (Maybe l) ->
  l ->
  Double ->
  m ()
observeHistogramWithLabel getMetricState alwaysObserve histogramVector label value = do
  recordMetricWithLabel
    getMetricState
    alwaysObserve
    (liftIO $ HistogramVector.observe histogramVector (Just label) value)
    (liftIO $ HistogramVector.observe histogramVector Nothing value)

-- | Record a subscription metric for all the operation names present in the subscription.
-- Use this when you want to update the same value of the metric for all the operation names.
recordSubcriptionMetric ::
  (MonadIO m) =>
  (IO GranularPrometheusMetricsState) ->
  -- should the metric be observed without a label when granularMetricsState is OFF
  Bool ->
  HashMap (Maybe OperationName) Int ->
  ParameterizedQueryHash ->
  SubscriptionKindLabel ->
  -- the mertic action to perform
  (SubscriptionLabel -> IO ()) ->
  m ()
recordSubcriptionMetric getMetricState alwaysObserve operationNamesMap parameterizedQueryHash subscriptionKind metricAction = do
  -- if no operation names are present, then emit metric with only param query hash as dynamic label
  if (null operationNamesMap)
    then do
      let promMetricGranularLabel = SubscriptionLabel subscriptionKind (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) Nothing)
          promMetricLabel = SubscriptionLabel subscriptionKind Nothing
      recordMetricWithLabel
        getMetricState
        alwaysObserve
        (metricAction promMetricGranularLabel)
        (metricAction promMetricLabel)
    else -- if operationNames are present, then emit the same metric for all the operation names
    do
      let operationNames = HashMap.keys operationNamesMap
      for_ operationNames $ \opName -> do
        let promMetricGranularLabel = SubscriptionLabel subscriptionKind (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) opName)
            promMetricLabel = SubscriptionLabel subscriptionKind Nothing
        recordMetricWithLabel
          getMetricState
          alwaysObserve
          (metricAction promMetricGranularLabel)
          (metricAction promMetricLabel)
