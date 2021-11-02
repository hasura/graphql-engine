{-# LANGUAGE PatternSynonyms #-}

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
    processEventQueue,
    defaultMaxEventThreads,
    defaultFetchInterval,
    defaultFetchBatchSize,
    Event (..),
    EventEngineCtx (..),
    LogBehavior (..),
    HeaderLogBehavior (..),
    ResponseLogBehavior (..),
    -- Exported for testing
    saveLockedEventTriggerEvents,
    removeEventTriggerEventFromLockedEvents,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended (Forever (..), sleep)
import Control.Concurrent.STM.TVar
import Control.Monad.Catch (MonadMask, bracket_, finally, mask_)
import Control.Monad.STM
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.TH
import Data.Has
import Data.HashMap.Strict qualified as M
import Data.Set qualified as Set
import Data.String
import Data.TByteString qualified as TBS
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Time.Clock
import Data.Time.Clock qualified as Time
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Base.Error
import Hasura.Eventing.Common
import Hasura.Eventing.HTTP
import Hasura.HTTP (getHTTPExceptionStatus)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.RequestTransform
import Hasura.RQL.Types
import Hasura.RQL.Types.Eventing.Backend
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Types
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import System.Metrics.Distribution qualified as EKG.Distribution
import System.Metrics.Gauge qualified as EKG.Gauge

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

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
    _eeCtxFetchSize :: NonNegativeInt
  }

data DeliveryInfo = DeliveryInfo
  { diCurrentRetry :: Int,
    diMaxRetries :: Int
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON {omitNothingFields = True} ''DeliveryInfo)

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
    epCreatedAt :: Time.UTCTime
  }
  deriving (Generic)

deriving instance Backend b => Show (EventPayload b)

deriving instance Backend b => Eq (EventPayload b)

instance Backend b => J.ToJSON (EventPayload b) where
  toJSON = J.genericToJSON hasuraJSON {omitNothingFields = True}

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultFetchInterval :: DiffTime
defaultFetchInterval = seconds 1

defaultFetchBatchSize :: NonNegativeInt
defaultFetchBatchSize = unsafeNonNegativeInt 100

initEventEngineCtx :: Int -> DiffTime -> NonNegativeInt -> STM EventEngineCtx
initEventEngineCtx maxT _eeCtxFetchInterval _eeCtxFetchSize = do
  _eeCtxEventThreadsCapacity <- newTVar maxT
  return $ EventEngineCtx {..}

saveLockedEventTriggerEvents :: MonadIO m => SourceName -> [EventId] -> TVar (HashMap SourceName (Set.Set EventId)) -> m ()
saveLockedEventTriggerEvents sourceName eventIds lockedEvents =
  liftIO $
    atomically $ do
      lockedEventsVals <- readTVar lockedEvents
      case M.lookup sourceName lockedEventsVals of
        Nothing -> writeTVar lockedEvents $! M.singleton sourceName (Set.fromList eventIds)
        Just _ -> writeTVar lockedEvents $! M.insertWith Set.union sourceName (Set.fromList eventIds) lockedEventsVals

removeEventTriggerEventFromLockedEvents ::
  MonadIO m => SourceName -> EventId -> TVar (HashMap SourceName (Set.Set EventId)) -> m ()
removeEventTriggerEventFromLockedEvents sourceName eventId lockedEvents =
  liftIO $
    atomically $ do
      lockedEventsVals <- readTVar lockedEvents
      writeTVar lockedEvents $! M.adjust (Set.delete eventId) sourceName lockedEventsVals

type BackendEventWithSource = AB.AnyBackend EventWithSource

type FetchEventArguments = ([BackendEventWithSource], Int, Bool)

pattern HttpErr :: e -> Either e (Either e' a)
pattern HttpErr e = Left e

pattern TransErr :: e' -> Either e (Either e' a)
pattern TransErr e = Right (Left e)

pattern Resp :: a -> Either e (Either e' a)
pattern Resp a = Right (Right a)

{-# COMPLETE HttpErr, TransErr, Resp #-}

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
    Tracing.HasReporter m,
    MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    MonadMask m
  ) =>
  L.Logger L.Hasura ->
  LogBehavior ->
  HTTP.Manager ->
  IO SchemaCache ->
  EventEngineCtx ->
  LockedEventsCtx ->
  ServerMetrics ->
  MaintenanceMode ->
  m (Forever m)
processEventQueue logger logBehavior httpMgr getSchemaCache EventEngineCtx {..} LockedEventsCtx {leEvents} serverMetrics maintenanceMode = do
  events0 <- popEventsBatch
  return $ Forever (events0, 0, False) go
  where
    fetchBatchSize = getNonNegativeInt _eeCtxFetchSize

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
      liftIO . fmap concat $
        -- fetch pending events across all the sources asynchronously
        LA.forConcurrently (M.toList allSources) \(sourceName, sourceCache) ->
          AB.dispatchAnyBackend @BackendEventTrigger sourceCache \(SourceInfo _sourceName tableCache _functionCache sourceConfig _queryTagsConfig _sourceCustomization :: SourceInfo b) -> do
            let tables = M.elems tableCache
            let eventTriggerCount = sum (M.size . _tiEventTriggerInfoMap <$> tables)

            -- only process events for this source if at least one event trigger exists
            if eventTriggerCount > 0
              then
                ( runExceptT (fetchUndeliveredEvents @b sourceConfig sourceName maintenanceMode (FetchBatchSize fetchBatchSize)) >>= \case
                    Right events -> do
                      _ <- liftIO $ EKG.Distribution.add (smNumEventsFetchedPerBatch serverMetrics) (fromIntegral $ length events)
                      eventsFetchedTime <- liftIO getCurrentTime
                      saveLockedEventTriggerEvents sourceName (eId <$> events) leEvents
                      return $ map (\event -> AB.mkAnyBackend @b $ EventWithSource event sourceConfig sourceName eventsFetchedTime) events
                    Left err -> do
                      liftIO $ L.unLogger logger $ EventInternalErr err
                      pure []
                )
              else pure []

    -- !!! CAREFUL !!!
    --     The logic here in particular is subtle and has been fixed, broken,
    --     and fixed again in several different ways, several times.
    -- !!! CAREFUL !!!
    --
    -- work on this batch of events while prefetching the next. Recurse after we've forked workers
    -- for each in the batch, minding the requested pool size.
    go :: FetchEventArguments -> m FetchEventArguments
    go (events, !fullFetchCount, !alreadyWarned) = do
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
              liftIO $
                atomically $ do
                  -- block until < HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE threads:
                  capacity <- readTVar _eeCtxEventThreadsCapacity
                  check $ capacity > 0
                  writeTVar _eeCtxEventThreadsCapacity (capacity - 1)
              -- since there is some capacity in our worker threads, we can launch another:
              let restoreCapacity =
                    liftIO $
                      atomically $
                        modifyTVar' _eeCtxEventThreadsCapacity (+ 1)
              t <-
                LA.async $
                  flip runReaderT (logger, httpMgr) $
                    processEvent eventWithSource'
                      `finally`
                      -- NOTE!: this needs to happen IN THE FORKED THREAD:
                      restoreCapacity
              LA.link t

        -- return when next batch ready; some 'processEvent' threads may be running.
        LA.wait eventsNextA

      let lenEvents = length events
      if
          | lenEvents == fetchBatchSize -> do
            -- If we've seen N fetches in a row from the DB come back full (i.e. only limited
            -- by our LIMIT clause), then we say we're clearly falling behind:
            let clearlyBehind = fullFetchCount >= 3
            unless alreadyWarned $
              when clearlyBehind $
                L.unLogger logger $
                  L.UnstructuredLog L.LevelWarn $
                    fromString $
                      "Events processor may not be keeping up with events generated in postgres, "
                        <> "or we're working on a backlog of events. Consider increasing "
                        <> "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
            return (eventsNext, (fullFetchCount + 1), (alreadyWarned || clearlyBehind))
          | otherwise -> do
            when (lenEvents /= fetchBatchSize && alreadyWarned) $
              -- emit as warning in case users are only logging warning severity and saw above
              L.unLogger logger $
                L.UnstructuredLog L.LevelWarn $
                  fromString $
                    "It looks like the events processor is keeping up again."
            return (eventsNext, 0, False)

    processEvent ::
      forall io r b.
      ( MonadIO io,
        MonadReader r io,
        Has HTTP.Manager r,
        Has (L.Logger L.Hasura) r,
        Tracing.HasReporter io,
        MonadMask io,
        BackendEventTrigger b
      ) =>
      EventWithSource b ->
      io ()
    processEvent (EventWithSource e sourceConfig sourceName eventFetchedTime) = do
      -- Track Queue Time of Event (in seconds). See `smEventQueueTime`
      -- Queue Time = Time when the event was fetched from DB - Time when the event is being processed
      eventProcessTime <- liftIO getCurrentTime
      let eventQueueTime = realToFrac $ diffUTCTime eventProcessTime eventFetchedTime
      _ <- liftIO $ EKG.Distribution.add (smEventQueueTime serverMetrics) eventQueueTime

      cache <- liftIO getSchemaCache

      tracingCtx <- liftIO (Tracing.extractEventContext (eEvent e))
      let spanName eti = "Event trigger: " <> unNonEmptyText (unTriggerName (etiName eti))
          runTraceT =
            maybe
              Tracing.runTraceT
              Tracing.runTraceTInContext
              tracingCtx

      maintenanceModeVersionEither :: Either QErr (Maybe MaintenanceModeVersion) <-
        case maintenanceMode of
          MaintenanceModeEnabled -> do
            runExceptT (getMaintenanceModeVersion @b sourceConfig) <&> \case
              Left err -> Left err
              Right maintenanceModeVersion -> Right $ Just maintenanceModeVersion
          MaintenanceModeDisabled -> return $ Right Nothing

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
            Right eti -> runTraceT (spanName eti) do
              let webhook = wciCachedValue $ etiWebhookInfo eti
                  retryConf = etiRetryConf eti
                  timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
                  httpTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
                  (headers, logHeaders) = prepareHeaders logBehavior (etiHeaders eti)
                  ep = createEventPayload retryConf e
                  payload = J.encode $ J.toJSON ep
                  extraLogCtx = ExtraLogContext (epId ep) (Just $ etiName eti)
                  dataTransform = mkRequestTransform <$> etiRequestTransform eti

              eitherRes <-
                runExceptT $
                  mkRequest headers httpTimeout payload dataTransform webhook >>= \case
                    Left err -> pure $ Left err
                    Right reqDetails -> do
                      let logger' res details = logHTTPForET res extraLogCtx details logBehavior
                      -- Event Triggers have a configuration parameter called
                      -- HASURA_GRAPHQL_EVENTS_HTTP_WORKERS, which is used
                      -- to control the concurrency of http delivery.
                      -- This bracket is used to increment and decrement an
                      -- HTTP Worker EKG Gauge for the duration of the
                      -- request invocation
                      resp <-
                        bracket_
                          (liftIO $ EKG.Gauge.inc $ smNumEventHTTPWorkers serverMetrics)
                          (liftIO $ EKG.Gauge.dec $ smNumEventHTTPWorkers serverMetrics)
                          (hoistEither =<< lift (invokeRequest reqDetails logger'))
                      pure $ Right resp
              case eitherRes of
                Resp resp -> processSuccess sourceConfig e logHeaders ep maintenanceModeVersion resp >>= flip onLeft logQErr
                HttpErr err -> processError @b sourceConfig e retryConf logHeaders ep maintenanceModeVersion err >>= flip onLeft logQErr
                TransErr err -> do
                  -- Log The Transformation Error
                  L.unLogger logger $ L.UnstructuredLog L.LevelError (TBS.fromLBS $ J.encode err)

                  -- Record an Event Error
                  recordError' @b sourceConfig e Nothing PESetError maintenanceModeVersion >>= flip onLeft logQErr
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
  EventPayload b ->
  Maybe MaintenanceModeVersion ->
  HTTPResp a ->
  m (Either QErr ())
processSuccess sourceConfig e reqHeaders ep maintenanceModeVersion resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation ep (Just respStatus) reqHeaders respBody respHeaders
  recordSuccess @b sourceConfig e invocation maintenanceModeVersion

processError ::
  forall b m a.
  ( MonadIO m,
    BackendEventTrigger b
  ) =>
  SourceConfig b ->
  Event b ->
  RetryConf ->
  [HeaderConf] ->
  EventPayload b ->
  Maybe MaintenanceModeVersion ->
  HTTPErr a ->
  m (Either QErr ())
processError sourceConfig e retryConf reqHeaders ep maintenanceModeVersion err = do
  let invocation = case err of
        HClient httpException ->
          let statusMaybe = getHTTPExceptionStatus httpException
           in mkInvocation ep statusMaybe reqHeaders (TBS.fromLBS (J.encode httpException)) []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation ep (Just respStatus) reqHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = TBS.fromLBS $ J.encode detail
          mkInvocation ep (Just 500) reqHeaders errMsg []
  retryOrError <- retryOrSetError e retryConf err
  recordError @b sourceConfig e invocation retryOrError maintenanceModeVersion

retryOrSetError ::
  MonadIO m =>
  Event b ->
  RetryConf ->
  HTTPErr a ->
  m ProcessEventError
retryOrSetError e retryConf err = do
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = mretryHeader >>= parseRetryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then pure PESetError
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
  Backend b =>
  EventPayload b ->
  Maybe Int ->
  [HeaderConf] ->
  TBS.TByteString ->
  [HeaderConf] ->
  Invocation 'EventType
mkInvocation eventPayload statusMaybe reqHeaders respBody respHeaders =
  let resp =
        case statusMaybe of
          Nothing -> mkClientErr respBody
          Just status ->
            if status >= 200 && status < 300
              then mkResp status respBody respHeaders
              else mkClientErr respBody
   in Invocation
        (epId eventPayload)
        statusMaybe
        (mkWebhookReq (J.toJSON eventPayload) reqHeaders invocationVersionET)
        resp

logQErr :: (MonadReader r m, Has (L.Logger L.Hasura) r, MonadIO m) => QErr -> m ()
logQErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLogger logger $ EventInternalErr err

getEventTriggerInfoFromEvent ::
  forall b. Backend b => SchemaCache -> Event b -> Either Text (EventTriggerInfo b)
getEventTriggerInfoFromEvent sc e = do
  let table = eTable e
      mTableInfo = unsafeTableInfo @b (eSource e) table $ scSources sc
  tableInfo <- onNothing mTableInfo $ Left ("table '" <> table <<> "' not found")
  let triggerName = tmName $ eTrigger e
      mEventTriggerInfo = M.lookup triggerName (_tiEventTriggerInfoMap tableInfo)
  onNothing mEventTriggerInfo $
    Left
      ( "event trigger '" <> triggerNameToTxt triggerName
          <> "' on table '"
          <> table <<> "' not found"
      )
