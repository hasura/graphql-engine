-- |
-- = Event Triggers
--
-- Event triggers are like ordinary SQL triggers, except instead of calling a SQL
-- procedure, they call a webhook. The event delivery mechanism involves coordination
-- between both the database and graphql-engine: only the SQL database knows
-- when the events should fire, but only graphql-engine know how to actually
-- deliver them.
--
-- Therefore, event triggers are implemented in two parts:
--
-- 1. Every event trigger is backed by a bona fide SQL trigger. When the SQL trigger
--    fires, it creates a new record in the hdb_catalog.event_log table.
--
-- 2. Concurrently, a thread in graphql-engine monitors the hdb_catalog.event_log
--    table for new events. When new event(s) are found, it uses the information
--    (URL,payload and headers) stored in the event to deliver the event
--    to the webhook.
--
-- The creation and deletion of SQL trigger itself is managed by the metadata DDL
-- APIs (see Hasura.RQL.DDL.EventTrigger), so this module focuses on event delivery.
--
-- Most of the subtleties involve guaranteeing reliable delivery of events:
-- we guarantee that every event will be delivered at least once,
-- even if graphql-engine crashes. This means we have to record the state
-- of each event in the database, and we have to retry
-- failed requests at a regular (user-configurable) interval.
module Hasura.Eventing.EventTrigger
  ( initEventEngineCtx,
    createFetchedEventsStatsLogger,
    closeFetchedEventsStatsLogger,
    processEventQueue,
    defaultMaxEventThreads,
    defaultFetchInterval,
    Event (..),
    EventEngineCtx (..),
    -- Exported for testing
    saveLockedEventTriggerEvents,
    removeEventTriggerEventFromLockedEvents,
    logQErr,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended (Forever (..), sleep)
import Control.Concurrent.STM.TVar
import Control.FoldDebounce qualified as FDebounce
import Control.Lens
import Control.Monad.Catch (MonadMask, bracket_, finally, mask_)
import Control.Monad.STM
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens qualified as JL
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.SerializableBlob qualified as SB
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Time qualified as Time
import Data.Time.Clock
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Base.Error
import Hasura.Eventing.Backend
import Hasura.Eventing.Common
import Hasura.Eventing.HTTP
import Hasura.HTTP (getHTTPExceptionStatus)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Prometheus
import Hasura.Server.Types
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Transformable qualified as HTTP
import Refined (NonNegative, Positive, Refined, unrefine)
import Refined.Unsafe (unsafeRefine)
import System.Metrics.Distribution qualified as EKG.Distribution
import System.Metrics.Gauge qualified as EKG.Gauge
import System.Metrics.Prometheus.Counter qualified as Prometheus.Counter
import System.Metrics.Prometheus.CounterVector (CounterVector)
import System.Metrics.Prometheus.CounterVector qualified as CounterVector
import System.Metrics.Prometheus.Gauge qualified as Prometheus.Gauge
import System.Metrics.Prometheus.Histogram qualified as Prometheus.Histogram
import System.Timeout.Lifted (timeout)

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Eq)

instance L.ToEngineLog EventInternalErr L.Hasura where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, L.eventTriggerLogType, J.toJSON qerr)

{- Note [Maintenance mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Maintenance mode is a mode in which users can upgrade their graphql-engine
without any down time. More on maintenance mode can be found here:
https://github.com/hasura/graphql-engine-mono/issues/431.

Basically, there are a few main things that maintenance mode boils down to:

1. No operation that may change the metadata will be allowed.
2. Migrations are not applied when the graphql-engine is started, so the
   catalog schema will be in the older version.
3. Event triggers should continue working in the new code with the older
   catalog schema i.e it should work even if there are any schema changes
   to the `hdb_catalog.event_log` table.

#1 and #2 are fairly self-explanatory. For #3, we need to support fetching
events depending upon the catalog version. So, fetch events works in the
following way now:

1. Check if maintenance mode is enabled
2. If maintenance mode is enabled then read the catalog version from the DB
   and accordingly fire the appropriate query to the events log table.
   When maintenance mode is disabled, we query the events log table according
   to the latest catalog, we do not read the catalog version for this.
-}

-- | See Note [Maintenance Mode]
data EventEngineCtx = EventEngineCtx
  { _eeCtxEventThreadsCapacity :: TVar Int,
    _eeCtxFetchInterval :: DiffTime,
    _eeCtxFetchSize :: Refined NonNegative Int
  }

data DeliveryInfo = DeliveryInfo
  { diCurrentRetry :: Int,
    diMaxRetries :: Int
  }
  deriving (Show, Generic, Eq)

instance J.ToJSON DeliveryInfo where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

newtype QualifiedTableStrict = QualifiedTableStrict
  { getQualifiedTable :: QualifiedTable
  }
  deriving (Show, Eq)

instance J.ToJSON QualifiedTableStrict where
  toJSON (QualifiedTableStrict (QualifiedObject sn tn)) =
    J.object
      [ "schema" J..= sn,
        "name" J..= tn
      ]

data EventPayload (b :: BackendType) = EventPayload
  { epId :: EventId,
    epTable :: TableName b,
    epTrigger :: TriggerMetadata,
    epEvent :: J.Value,
    epDeliveryInfo :: DeliveryInfo,
    epCreatedAt :: Time.LocalTime
  }
  deriving (Generic)

deriving instance (Backend b) => Show (EventPayload b)

deriving instance (Backend b) => Eq (EventPayload b)

instance (Backend b) => J.ToJSON (EventPayload b) where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}

defaultMaxEventThreads :: Refined Positive Int
defaultMaxEventThreads = unsafeRefine 100

defaultFetchInterval :: DiffTime
defaultFetchInterval = seconds 1

initEventEngineCtx :: (MonadIO m) => Refined Positive Int -> Refined NonNegative Milliseconds -> Refined NonNegative Int -> m EventEngineCtx
initEventEngineCtx maxThreads fetchInterval _eeCtxFetchSize = do
  _eeCtxEventThreadsCapacity <- liftIO $ newTVarIO $ unrefine maxThreads
  let _eeCtxFetchInterval = milliseconds $ unrefine fetchInterval
  return EventEngineCtx {..}

saveLockedEventTriggerEvents :: (MonadIO m) => SourceName -> [EventId] -> TVar (HashMap SourceName (Set.Set EventId)) -> m ()
saveLockedEventTriggerEvents sourceName eventIds lockedEvents =
  liftIO
    $ atomically
    $ do
      lockedEventsVals <- readTVar lockedEvents
      case HashMap.lookup sourceName lockedEventsVals of
        Nothing -> writeTVar lockedEvents $! HashMap.singleton sourceName (Set.fromList eventIds)
        Just _ -> writeTVar lockedEvents $! HashMap.insertWith Set.union sourceName (Set.fromList eventIds) lockedEventsVals

removeEventTriggerEventFromLockedEvents ::
  (MonadIO m) => SourceName -> EventId -> TVar (HashMap SourceName (Set.Set EventId)) -> m ()
removeEventTriggerEventFromLockedEvents sourceName eventId lockedEvents =
  liftIO
    $ atomically
    $ do
      lockedEventsVals <- readTVar lockedEvents
      writeTVar lockedEvents $! HashMap.adjust (Set.delete eventId) sourceName lockedEventsVals

type BackendEventWithSource = AB.AnyBackend EventWithSource

type FetchEventArguments = ([BackendEventWithSource], Int, Bool)

newtype EventsCount = EventsCount {unEventsCount :: Int}
  deriving (Eq, Show, J.ToJSON, J.FromJSON, Num)

newtype NumEventsFetchedPerSource = NumEventsFetchedPerSource {unNumEventsFetchedPerSource :: HashMap SourceName EventsCount}
  deriving (Eq, Show)

instance J.ToJSON NumEventsFetchedPerSource where
  toJSON (NumEventsFetchedPerSource m) =
    J.Object $ KeyMap.fromList $ map ((Key.fromText . sourceNameToText) *** J.toJSON) $ HashMap.toList m

instance Semigroup NumEventsFetchedPerSource where
  (NumEventsFetchedPerSource lMap) <> (NumEventsFetchedPerSource rMap) =
    NumEventsFetchedPerSource $ HashMap.unionWith (+) lMap rMap

instance Monoid NumEventsFetchedPerSource where
  mempty = NumEventsFetchedPerSource mempty

data FetchedEventsStats = FetchedEventsStats
  { _fesNumEventsFetched :: NumEventsFetchedPerSource,
    _fesNumFetches :: Int
  }
  deriving (Eq, Generic, Show)

instance J.ToJSON FetchedEventsStats where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance L.ToEngineLog FetchedEventsStats L.Hasura where
  toEngineLog stats =
    (L.LevelInfo, L.eventTriggerProcessLogType, J.toJSON stats)

instance Semigroup FetchedEventsStats where
  (FetchedEventsStats lMap lFetches) <> (FetchedEventsStats rMap rFetches) =
    FetchedEventsStats (lMap <> rMap) (lFetches + rFetches)

instance Monoid FetchedEventsStats where
  mempty = FetchedEventsStats mempty 0

type FetchedEventsStatsLogger = FDebounce.Trigger FetchedEventsStats FetchedEventsStats

-- | Logger to accumulate stats of fetched events over a period of time and log once using @'L.Logger L.Hasura'.
-- See @'createStatsLogger' for more details.
createFetchedEventsStatsLogger :: (MonadIO m) => L.Logger L.Hasura -> m FetchedEventsStatsLogger
createFetchedEventsStatsLogger = L.createStatsLogger

-- | Close the fetched events stats logger.
closeFetchedEventsStatsLogger :: (MonadIO m) => L.Logger L.Hasura -> FetchedEventsStatsLogger -> m ()
closeFetchedEventsStatsLogger = L.closeStatsLogger L.eventTriggerProcessLogType

-- | Log statistics of fetched events. See @'logStats' for more details.
logFetchedEventsStatistics ::
  (MonadIO m) =>
  FetchedEventsStatsLogger ->
  [BackendEventWithSource] ->
  m ()
logFetchedEventsStatistics logger backendEvents =
  L.logStats logger (FetchedEventsStats numEventsFetchedPerSource 1)
  where
    numEventsFetchedPerSource =
      let sourceNames = flip map backendEvents
            $ \backendEvent -> AB.dispatchAnyBackend @Backend backendEvent _ewsSourceName
       in NumEventsFetchedPerSource $ HashMap.fromListWith (+) [(sourceName, 1) | sourceName <- sourceNames]

{-# ANN processEventQueue ("HLint: ignore Use withAsync" :: String) #-}

-- | `upperBoundEventTriggerTimeout` is the maximum amount of time
--    an event trigger can take to process. This function is intended
--    to use with a timeout.
upperBoundEventTriggerTimeout :: DiffTime
upperBoundEventTriggerTimeout = minutes 30

-- | Service events from our in-DB queue.
--
-- There are a few competing concerns and constraints here; we want to...
--   - fetch events in batches for lower DB pressure
--   - don't fetch more than N at a time (since that can mean: space leak, less
--     effective scale out, possible double sends for events we've checked out
--     on exit (TODO clean shutdown procedure))
--   - try not to cause webhook workers to stall waiting on DB fetch
--   - limit webhook HTTP concurrency per HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE
processEventQueue ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    MonadMask m,
    Tracing.MonadTrace m,
    MonadGetPolicies m
  ) =>
  L.Logger L.Hasura ->
  FetchedEventsStatsLogger ->
  HTTP.Manager ->
  IO SchemaCache ->
  IO EventEngineCtx ->
  TVar Int ->
  LockedEventsCtx ->
  ServerMetrics ->
  EventTriggerMetrics ->
  MaintenanceMode () ->
  TriggersErrorLogLevelStatus ->
  m (Forever m)
processEventQueue logger statsLogger httpMgr getSchemaCache getEventEngineCtx activeEventProcessingThreads LockedEventsCtx {leEvents} serverMetrics eventTriggerMetrics maintenanceMode triggersErrorLogLevelStatus = do
  events0 <- popEventsBatch
  return $ Forever (events0, 0, False) go
  where
    popEventsBatch :: m [BackendEventWithSource]
    popEventsBatch = do
      {-
        SELECT FOR UPDATE .. SKIP LOCKED can throw serialization errors in RepeatableRead: https://stackoverflow.com/a/53289263/1911889
        We can avoid this safely by running it in ReadCommitted as Postgres will recheck the
        predicate condition if a row is updated concurrently: https://www.postgresql.org/docs/9.5/transaction-iso.html#XACT-READ-COMMITTED

        Every other action on an event_log row (like post-processing, archival, etc) are single writes (no R-W or W-R)
        so it is safe to perform them in ReadCommitted as well (the writes will then acquire some serial order).
        Any serial order of updates to a row will lead to an eventually consistent state as the row will have
        (delivered=t or error=t or archived=t) after a fixed number of tries (assuming it begins with locked='f').
      -}
      allSources <- scSources <$> liftIO getSchemaCache
      fetchBatchSize <- unrefine . _eeCtxFetchSize <$> liftIO getEventEngineCtx
      events <- liftIO
        . fmap concat
        $
        -- fetch pending events across all the sources asynchronously
        LA.forConcurrently (HashMap.toList allSources) \(sourceName, sourceCache) ->
          AB.dispatchAnyBackend @BackendEventTrigger sourceCache \(SourceInfo {..} :: SourceInfo b) -> do
            let tables = HashMap.elems _siTables
                triggerMap = _tiEventTriggerInfoMap <$> tables
                eventTriggerCount = sum (HashMap.size <$> triggerMap)
                triggerNames = concatMap HashMap.keys triggerMap

            -- only process events for this source if at least one event trigger exists
            if eventTriggerCount > 0
              then do
                eventPollStartTime <- getCurrentTime
                runExceptT (fetchUndeliveredEvents @b _siConfiguration sourceName triggerNames maintenanceMode (FetchBatchSize fetchBatchSize)) >>= \case
                  Right events -> do
                    let eventFetchCount = fromIntegral $ length events
                    Prometheus.Gauge.set (eventsFetchedPerBatch eventTriggerMetrics) eventFetchCount
                    if (null events)
                      then return []
                      else do
                        eventsFetchedTime <- getCurrentTime -- This is also the poll end time
                        let eventPollTime = realToFrac $ diffUTCTime eventsFetchedTime eventPollStartTime
                        _ <- EKG.Distribution.add (smEventFetchTimePerBatch serverMetrics) eventPollTime
                        Prometheus.Histogram.observe (eventsFetchTimePerBatch eventTriggerMetrics) eventPollTime
                        _ <- EKG.Distribution.add (smNumEventsFetchedPerBatch serverMetrics) (fromIntegral $ length events)
                        saveLockedEventTriggerEvents sourceName (eId <$> events) leEvents
                        return $ map (\event -> AB.mkAnyBackend @b $ EventWithSource event _siConfiguration sourceName eventsFetchedTime) events
                  Left err -> do
                    L.unLogger logger $ EventInternalErr err
                    pure []
              else pure []

      -- Log the statistics of events fetched
      logFetchedEventsStatistics statsLogger events
      pure events

    -- !!! CAREFUL !!!
    --     The logic here in particular is subtle and has been fixed, broken,
    --     and fixed again in several different ways, several times.
    -- !!! CAREFUL !!!
    --
    -- work on this batch of events while prefetching the next. Recurse after we've forked workers
    -- for each in the batch, minding the requested pool size.
    go :: FetchEventArguments -> m FetchEventArguments
    go (events, !fullFetchCount, !alreadyWarned) = do
      EventEngineCtx {..} <- liftIO getEventEngineCtx
      let fetchBatchSize = unrefine _eeCtxFetchSize
      -- process events ASAP until we've caught up; only then can we sleep
      when (null events) . liftIO $ sleep _eeCtxFetchInterval

      -- Prefetch next events payload while concurrently working through our current batch.
      -- NOTE: we probably don't need to prefetch so early, but probably not
      -- worth the effort for something more fine-tuned
      eventsNext <- LA.withAsync popEventsBatch $ \eventsNextA -> do
        -- process approximately in order, minding HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE:
        forM_ events $ \eventWithSource ->
          -- NOTE: we implement a logical bracket pattern here with the
          -- increment and decrement of _eeCtxEventThreadsCapacity which
          -- depends on not putting anything that can throw in the body here:
          AB.dispatchAnyBackend @BackendEventTrigger eventWithSource \(eventWithSource' :: EventWithSource b) ->
            mask_ $ do
              liftIO
                $ atomically
                $ do
                  -- block until < HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE threads:
                  maxCapacity <- readTVar _eeCtxEventThreadsCapacity
                  activeThreadCount <- readTVar activeEventProcessingThreads
                  check $ maxCapacity > activeThreadCount
                  modifyTVar' activeEventProcessingThreads (+ 1)
              -- since there is some capacity in our worker threads, we can launch another:
              t <-
                LA.async
                  $ flip runReaderT (logger, httpMgr)
                  $ processEvent eventWithSource'
                  `finally`
                  -- NOTE!: this needs to happen IN THE FORKED THREAD:
                  decrementActiveThreadCount
              LA.link t

        -- return when next batch ready; some 'processEvent' threads may be running.
        LA.wait eventsNextA

      let lenEvents = length events
      if
        | lenEvents == fetchBatchSize -> do
            -- If we've seen N fetches in a row from the DB come back full (i.e. only limited
            -- by our LIMIT clause), then we say we're clearly falling behind:
            let clearlyBehind = fullFetchCount >= 3
            unless alreadyWarned
              $ when clearlyBehind
              $ L.unLoggerTracing logger
              $ L.UnstructuredLog L.LevelWarn
              $ fromString
              $ "Events processor may not be keeping up with events generated in postgres, "
              <> "or we're working on a backlog of events. Consider increasing "
              <> "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
            return (eventsNext, (fullFetchCount + 1), (alreadyWarned || clearlyBehind))
        | otherwise -> do
            when (lenEvents /= fetchBatchSize && alreadyWarned)
              $
              -- emit as warning in case users are only logging warning severity and saw above
              L.unLoggerTracing logger
              $ L.UnstructuredLog L.LevelWarn
              $ fromString
              $ "It looks like the events processor is keeping up again."
            return (eventsNext, 0, False)

    decrementActiveThreadCount =
      liftIO
        $ atomically
        $ modifyTVar' activeEventProcessingThreads (subtract 1)

    -- \| Extract a trace context from an event trigger payload.
    extractEventContext :: forall io. (MonadIO io) => J.Value -> io (Maybe Tracing.TraceContext)
    extractEventContext e = do
      let traceIdMaybe =
            Tracing.traceIdFromHex
              . txtToBs
              =<< e
              ^? JL.key "trace_context" . JL.key "trace_id" . JL._String
      for traceIdMaybe $ \traceId -> do
        freshSpanId <- Tracing.randomSpanId
        let parentSpanId =
              Tracing.spanIdFromHex
                . txtToBs
                =<< e
                ^? JL.key "trace_context" . JL.key "span_id" . JL._String
            samplingState =
              Tracing.samplingStateFromHeader
                $ e
                ^? JL.key "trace_context" . JL.key "sampling_state" . JL._String
        pure $ Tracing.TraceContext traceId freshSpanId parentSpanId samplingState Tracing.emptyTraceState

    processEvent ::
      forall io r b.
      ( MonadBaseControl IO io,
        MonadIO io,
        MonadReader r io,
        Has HTTP.Manager r,
        Has (L.Logger L.Hasura) r,
        MonadMask io,
        BackendEventTrigger b,
        Tracing.MonadTrace io,
        MonadGetPolicies io
      ) =>
      EventWithSource b ->
      io ()
    processEvent (EventWithSource e sourceConfig sourceName eventFetchedTime) = do
      -- IO action for fetching the configured metrics granularity
      getPrometheusMetricsGranularity <- runGetPrometheusMetricsGranularity
      -- Track Queue Time of Event (in seconds). See `smEventQueueTime`
      -- Queue Time = Time when the event was fetched from DB - Time when the event is being processed
      eventProcessTime <- liftIO getCurrentTime
      let eventQueueTime = realToFrac $ diffUTCTime eventProcessTime eventFetchedTime
      _ <- liftIO $ EKG.Distribution.add (smEventQueueTime serverMetrics) eventQueueTime
      liftIO
        $ observeHistogramWithLabel
          getPrometheusMetricsGranularity
          True
          (eventQueueTimeSeconds eventTriggerMetrics)
          (DynamicEventTriggerLabel (tmName (eTrigger e)) sourceName)
          eventQueueTime

      cache <- liftIO getSchemaCache

      trace <-
        extractEventContext (eEvent e) <&> \case
          Nothing -> Tracing.newTrace Tracing.sampleAlways
          Just ctx -> Tracing.newTraceWith ctx Tracing.sampleAlways
      let spanName eti = "Event trigger: " <> unNonEmptyText (unTriggerName (etiName eti))

      maintenanceModeVersionEither :: Either QErr (MaintenanceMode MaintenanceModeVersion) <-
        case maintenanceMode of
          MaintenanceModeEnabled () -> do
            runExceptT (getMaintenanceModeVersion @b sourceConfig) <&> \case
              Left err -> Left err
              Right maintenanceModeVersion -> Right $ (MaintenanceModeEnabled maintenanceModeVersion)
          MaintenanceModeDisabled -> return $ Right MaintenanceModeDisabled

      case maintenanceModeVersionEither of
        Left maintenanceModeVersionErr -> logQErr maintenanceModeVersionErr
        Right maintenanceModeVersion ->
          case getEventTriggerInfoFromEvent cache e of
            Left err -> do
              --  This rare error can happen in the following known cases:
              --  i) schema cache is not up-to-date (due to some bug, say during schema syncing across multiple instances)
              --  ii) the event trigger is dropped when this event was just fetched
              logQErr $ err500 Unexpected err
              currentTime <- liftIO getCurrentTime
              -- For such an event, we unlock the event and retry after a minute
              runExceptT (setRetry sourceConfig e (addUTCTime 60 currentTime) maintenanceModeVersion)
                >>= flip onLeft logQErr
            Right eti -> trace (spanName eti) do
              let webhook = wciCachedValue $ etiWebhookInfo eti
                  retryConf = etiRetryConf eti
                  timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
                  httpTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
                  (headers, logHeaders) = prepareHeaders (etiHeaders eti)
                  ep = createEventPayload retryConf e
                  payload = J.encode $ J.toJSON ep
                  extraLogCtx = ExtraLogContext (epId ep) (Just $ etiName eti)
                  requestTransform = etiRequestTransform eti
                  responseTransform = mkResponseTransform <$> etiResponseTransform eti
                  eventTriggerProcessingTimeout = maybe upperBoundEventTriggerTimeout (min upperBoundEventTriggerTimeout . fromIntegral) (rcTimeoutSec retryConf)
                  eventTriggerProcessAction = do
                    eventExecutionStartTime <- liftIO getCurrentTime
                    eitherReqRes <-
                      runExceptT
                        $ mkRequest headers httpTimeout payload requestTransform (_envVarValue webhook)
                        >>= \reqDetails -> do
                          let request = extractRequest reqDetails
                              tracesPropagator = getOtelTracesPropagator $ scOpenTelemetryConfig cache
                              logger' res details = do
                                logHTTPForET res extraLogCtx details (_envVarName webhook) logHeaders triggersErrorLogLevelStatus
                                liftIO $ do
                                  case res of
                                    Left _err -> pure ()
                                    Right response ->
                                      Prometheus.Counter.add
                                        (eventTriggerBytesReceived eventTriggerMetrics)
                                        (hrsSize response)
                                  let RequestDetails {_rdOriginalSize, _rdTransformedSize} = details
                                   in Prometheus.Counter.add
                                        (eventTriggerBytesSent eventTriggerMetrics)
                                        (fromMaybe _rdOriginalSize _rdTransformedSize)
                          -- Event Triggers have a configuration parameter called
                          -- HASURA_GRAPHQL_EVENTS_HTTP_WORKERS, which is used
                          -- to control the concurrency of http delivery.
                          -- This bracket is used to increment and decrement an
                          -- HTTP Worker EKG Gauge for the duration of the
                          -- request invocation
                          resp <-
                            bracket_
                              ( do
                                  liftIO $ EKG.Gauge.inc $ smNumEventHTTPWorkers serverMetrics
                                  liftIO $ Prometheus.Gauge.inc (eventTriggerHTTPWorkers eventTriggerMetrics)
                              )
                              ( do
                                  liftIO $ EKG.Gauge.dec $ smNumEventHTTPWorkers serverMetrics
                                  liftIO $ Prometheus.Gauge.dec (eventTriggerHTTPWorkers eventTriggerMetrics)
                              )
                              (invokeRequest reqDetails responseTransform (_rdSessionVars reqDetails) logger' tracesPropagator)
                          pure (request, resp)
                    case eitherReqRes of
                      Right (req, resp) -> do
                        let reqBody = fromMaybe J.Null $ preview (HTTP.body . HTTP._RequestBodyLBS) req >>= J.decode @J.Value
                        processSuccess sourceConfig e logHeaders reqBody maintenanceModeVersion resp >>= flip onLeft logQErr
                        eventExecutionFinishTime <- liftIO getCurrentTime
                        let eventWebhookProcessingTime' = realToFrac $ diffUTCTime eventExecutionFinishTime eventExecutionStartTime
                            -- For event_processing_time, the start time is defined as the expected delivery time for an event, i.e.:
                            --  - For event with no retries: created_at (at UTC) time
                            --  - For event with retries: next_retry_at (at UTC) time

                            -- The timestamps in the DB are supposed to be UTC time, so the timestamps (`eventExecutionFinishTime` and
                            -- `eventStartTime`) used here in calculation are all UTC time.
                            eventStartTime = fromMaybe (eCreatedAtUTC e) (eRetryAtUTC e)
                            eventProcessingTime' = realToFrac $ diffUTCTime eventExecutionFinishTime eventStartTime
                        observeHistogramWithLabel
                          getPrometheusMetricsGranularity
                          True
                          (eventProcessingTime eventTriggerMetrics)
                          (DynamicEventTriggerLabel (etiName eti) sourceName)
                          eventProcessingTime'
                        liftIO $ do
                          EKG.Distribution.add (smEventWebhookProcessingTime serverMetrics) eventWebhookProcessingTime'
                          observeHistogramWithLabel
                            getPrometheusMetricsGranularity
                            True
                            (eventWebhookProcessingTime eventTriggerMetrics)
                            (DynamicEventTriggerLabel (etiName eti) sourceName)
                            eventWebhookProcessingTime'
                          EKG.Distribution.add (smEventProcessingTime serverMetrics) eventProcessingTime'
                          incEventTriggerCounterWithLabel
                            getPrometheusMetricsGranularity
                            True
                            (eventProcessedTotal eventTriggerMetrics)
                            (EventStatusWithTriggerLabel eventSuccessLabel (Just (DynamicEventTriggerLabel (etiName eti) sourceName)))
                          incEventTriggerCounterWithLabel
                            getPrometheusMetricsGranularity
                            True
                            (eventInvocationTotal eventTriggerMetrics)
                            (EventStatusWithTriggerLabel eventSuccessLabel (Just (DynamicEventTriggerLabel (etiName eti) sourceName)))
                      Left eventError -> do
                        -- TODO (paritosh): We can also add a label to the metric to indicate the type of error
                        liftIO
                          $ incEventTriggerCounterWithLabel
                            getPrometheusMetricsGranularity
                            True
                            (eventInvocationTotal eventTriggerMetrics)
                            (EventStatusWithTriggerLabel eventFailedLabel (Just (DynamicEventTriggerLabel (etiName eti) sourceName)))
                        case eventError of
                          (HTTPError reqBody err) ->
                            processError @b sourceConfig e retryConf logHeaders reqBody maintenanceModeVersion eventTriggerMetrics err >>= flip onLeft logQErr
                          (TransformationError _ err) -> do
                            L.unLoggerTracing logger $ L.UnstructuredLog L.LevelError (SB.fromLBS $ J.encode err)

                            -- Record an Event Error
                            recordError' @b sourceConfig e Nothing PESetError maintenanceModeVersion >>= flip onLeft logQErr
              -- Try to process the event trigger with a timeout of min(`uppserBoundEventTriggerTimeout`, event's response timeout),
              -- so that we're never blocked forever while processing a single event trigger.
              --
              -- If the request times out, then process it as an erroneous invocation and move on.
              timeout (fromInteger (diffTimeToMicroSeconds eventTriggerProcessingTimeout)) eventTriggerProcessAction
                `onNothingM` do
                  let eventTriggerTimeoutMessage = "Event Trigger " <> etiName eti <<> " timed out while processing."
                      eventTriggerTimeoutError = err500 TimeoutErrorCode eventTriggerTimeoutMessage
                  L.unLoggerTracing logger $ EventInternalErr eventTriggerTimeoutError
                  processError @b sourceConfig e retryConf logHeaders J.Null maintenanceModeVersion eventTriggerMetrics (HOther $ T.unpack eventTriggerTimeoutMessage)
                    >>= flip onLeft logQErr

      -- removing an event from the _eeCtxLockedEvents after the event has been processed:
      removeEventTriggerEventFromLockedEvents sourceName (eId e) leEvents

createEventPayload :: RetryConf -> Event b -> EventPayload b
createEventPayload retryConf e =
  EventPayload
    { epId = eId e,
      epTable = eTable e,
      epTrigger = eTrigger e,
      epEvent = eEvent e,
      epDeliveryInfo =
        DeliveryInfo
          { diCurrentRetry = eTries e,
            diMaxRetries = rcNumRetries retryConf
          },
      epCreatedAt = eCreatedAt e
    }

processSuccess ::
  forall b m a.
  (MonadIO m, BackendEventTrigger b) =>
  SourceConfig b ->
  Event b ->
  [HeaderConf] ->
  J.Value ->
  MaintenanceMode MaintenanceModeVersion ->
  HTTPResp a ->
  m (Either QErr ())
processSuccess sourceConfig e reqHeaders ep maintenanceModeVersion resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      eid = eId e
      invocation = mkInvocation eid ep (Just respStatus) reqHeaders respBody respHeaders
  recordSuccess @b sourceConfig e invocation maintenanceModeVersion

processError ::
  forall b m a.
  ( MonadIO m,
    BackendEventTrigger b,
    MonadGetPolicies m
  ) =>
  SourceConfig b ->
  Event b ->
  RetryConf ->
  [HeaderConf] ->
  J.Value ->
  MaintenanceMode MaintenanceModeVersion ->
  EventTriggerMetrics ->
  HTTPErr a ->
  m (Either QErr ())
processError sourceConfig e retryConf reqHeaders ep maintenanceModeVersion eventTriggerMetrics err = do
  let invocation = case err of
        HClient httpException ->
          let statusMaybe = getHTTPExceptionStatus httpException
           in mkInvocation (eId e) ep statusMaybe reqHeaders (SB.fromLBS (httpExceptionErrorEncoding httpException)) []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation (eId e) ep (Just respStatus) reqHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = SB.fromLBS $ J.encode detail
          mkInvocation (eId e) ep (Just 500) reqHeaders errMsg []
  retryOrError <- retryOrSetError e retryConf eventTriggerMetrics err
  recordError @b sourceConfig e invocation retryOrError maintenanceModeVersion

retryOrSetError ::
  ( MonadIO m,
    MonadGetPolicies m
  ) =>
  Event b ->
  RetryConf ->
  EventTriggerMetrics ->
  HTTPErr a ->
  m ProcessEventError
retryOrSetError e retryConf eventTriggerMetrics err = do
  getPrometheusMetricsGranularity <- runGetPrometheusMetricsGranularity
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = mretryHeader >>= parseRetryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      liftIO
        $ incEventTriggerCounterWithLabel
          getPrometheusMetricsGranularity
          True
          (eventProcessedTotal eventTriggerMetrics)
          (EventStatusWithTriggerLabel eventFailedLabel (Just (DynamicEventTriggerLabel (tmName (eTrigger e)) (eSource e))))
      pure PESetError
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      pure $ PESetRetry retryTime
  where
    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _ = Nothing

    parseRetryHeader = mfilter (> 0) . readMaybe . T.unpack

mkInvocation ::
  EventId ->
  J.Value ->
  Maybe Int ->
  [HeaderConf] ->
  SB.SerializableBlob ->
  [HeaderConf] ->
  Invocation 'EventType
mkInvocation eid ep statusMaybe reqHeaders respBody respHeaders =
  let resp =
        case statusMaybe of
          Nothing -> mkClientErr respBody
          Just status ->
            if status >= 200 && status < 300
              then mkResp status respBody respHeaders
              else mkClientErr respBody
   in Invocation
        eid
        statusMaybe
        (mkWebhookReq ep reqHeaders invocationVersionET)
        resp

logQErr :: (Tracing.MonadTraceContext m, MonadReader r m, Has (L.Logger L.Hasura) r, MonadIO m) => QErr -> m ()
logQErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLoggerTracing logger $ EventInternalErr err

getEventTriggerInfoFromEvent ::
  forall b. (Backend b) => SchemaCache -> Event b -> Either Text (EventTriggerInfo b)
getEventTriggerInfoFromEvent sc e = do
  let table = eTable e
      mTableInfo = unsafeTableInfo @b (eSource e) table $ scSources sc
  tableInfo <- onNothing mTableInfo $ Left ("table '" <> table <<> "' not found")
  let triggerName = tmName $ eTrigger e
      mEventTriggerInfo = HashMap.lookup triggerName (_tiEventTriggerInfoMap tableInfo)
  onNothing mEventTriggerInfo
    $ Left
      ( "event trigger '"
          <> triggerNameToTxt triggerName
          <> "' on table '"
          <> table
          <<> "' not found"
      )

incEventTriggerCounterWithLabel ::
  (MonadIO m) =>
  (IO GranularPrometheusMetricsState) ->
  -- should the metric be observed without a label when granularMetricsState is OFF
  Bool ->
  CounterVector EventStatusWithTriggerLabel ->
  EventStatusWithTriggerLabel ->
  m ()
incEventTriggerCounterWithLabel getMetricState alwaysObserve counterVector (EventStatusWithTriggerLabel status tl) = do
  recordMetricWithLabel
    getMetricState
    alwaysObserve
    (liftIO $ CounterVector.inc counterVector (EventStatusWithTriggerLabel status tl))
    (liftIO $ CounterVector.inc counterVector (EventStatusWithTriggerLabel status Nothing))
