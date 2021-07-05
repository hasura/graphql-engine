{-|
= Event Triggers

Event triggers are like ordinary SQL triggers, except instead of calling a SQL
procedure, they call a webhook. The event delivery mechanism involves coordination
between both the database and graphql-engine: only the SQL database knows
when the events should fire, but only graphql-engine know how to actually
deliver them.

Therefore, event triggers are implemented in two parts:

1. Every event trigger is backed by a bona fide SQL trigger. When the SQL trigger
   fires, it creates a new record in the hdb_catalog.event_log table.

2. Concurrently, a thread in graphql-engine monitors the hdb_catalog.event_log
   table for new events. When new event(s) are found, it uses the information
   (URL,payload and headers) stored in the event to deliver the event
   to the webhook.

The creation and deletion of SQL trigger itself is managed by the metadata DDL
APIs (see Hasura.RQL.DDL.EventTrigger), so this module focuses on event delivery.

Most of the subtleties involve guaranteeing reliable delivery of events:
we guarantee that every event will be delivered at least once,
even if graphql-engine crashes. This means we have to record the state
of each event in the database, and we have to retry
failed requests at a regular (user-configurable) interval.

-}

{-# LANGUAGE StrictData #-}

module Hasura.Eventing.EventTrigger
  ( initEventEngineCtx
  , processEventQueue
  , defaultMaxEventThreads
  , defaultFetchInterval
  , defaultFetchBatchSize
  , Event(..)
  , unlockEvents
  , EventEngineCtx(..)
  , LogBehavior(..)
  , HeaderLogBehavior(..)
  , ResponseLogBehavior(..)
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe   as LA
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as M
import qualified Data.TByteString                       as TBS
import qualified Data.Text                              as T
import qualified Data.Time.Clock                        as Time
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified System.Metrics.Distribution            as EKG.Distribution
import qualified System.Metrics.Gauge                   as EKG.Gauge

import           Control.Concurrent.Extended            (Forever (..), sleep)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch                    (MonadMask, bracket_, finally, mask_)
import           Control.Monad.STM
import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Has
import           Data.Int                               (Int64)
import           Data.String
import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Data.Time.Clock

import qualified Hasura.Logging                         as L
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Backends.Postgres.Execute.Types
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Base.Error
import           Hasura.Eventing.Common
import           Hasura.Eventing.HTTP
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Migrate.Internal         (getCatalogVersion)
import           Hasura.Server.Migrate.Version          (latestCatalogVersionString)
import           Hasura.Server.Types
import           Hasura.Server.Version                  (HasVersion)


data TriggerMetadata
  = TriggerMetadata { tmName :: TriggerName }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON{omitNothingFields=True} ''TriggerMetadata)

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr L.Hasura where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, L.eventTriggerLogType, toJSON qerr)

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
--
data MaintenanceModeVersion
  = PreviousMMVersion
  -- ^ should correspond to the catalog version from which the user
  -- is migrating from
  | CurrentMMVersion
  -- ^ should correspond to the latest catalog version
  deriving (Show, Eq)

-- | Change data for a particular row
--
-- https://docs.hasura.io/1.0/graphql/manual/event-triggers/payload.html
data Event
  = Event
  { eId        :: !EventId
  , eSource    :: !SourceName
  , eTable     :: !QualifiedTable
  , eTrigger   :: !TriggerMetadata
  , eEvent     :: !Value
  , eTries     :: !Int
  , eCreatedAt :: !Time.UTCTime
  } deriving (Show, Eq)

$(deriveFromJSON hasuraJSON{omitNothingFields=True} ''Event)

data EventEngineCtx
  = EventEngineCtx
  { _eeCtxEventThreadsCapacity :: TVar Int
  , _eeCtxFetchInterval        :: DiffTime
  , _eeCtxFetchSize            :: NonNegativeInt
  }

data DeliveryInfo
  = DeliveryInfo
  { diCurrentRetry :: Int
  , diMaxRetries   :: Int
  } deriving (Show, Eq)

$(deriveJSON hasuraJSON{omitNothingFields=True} ''DeliveryInfo)

newtype QualifiedTableStrict = QualifiedTableStrict
  { getQualifiedTable :: QualifiedTable
  } deriving (Show, Eq)

instance ToJSON QualifiedTableStrict where
  toJSON (QualifiedTableStrict (QualifiedObject sn tn)) =
     object [ "schema" .= sn
            , "name"  .= tn
           ]

data EventPayload
  = EventPayload
  { epId           :: EventId
  , epTable        :: QualifiedTableStrict
  , epTrigger      :: TriggerMetadata
  , epEvent        :: Value
  , epDeliveryInfo :: DeliveryInfo
  , epCreatedAt    :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveToJSON hasuraJSON{omitNothingFields=True} ''EventPayload)

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultFetchInterval :: DiffTime
defaultFetchInterval = seconds 1

defaultFetchBatchSize :: NonNegativeInt
defaultFetchBatchSize = unsafeNonNegativeInt 100

initEventEngineCtx :: Int -> DiffTime -> NonNegativeInt -> STM EventEngineCtx
initEventEngineCtx maxT _eeCtxFetchInterval _eeCtxFetchSize = do
  _eeCtxEventThreadsCapacity <- newTVar maxT
  return $ EventEngineCtx{..}

-- | The event payload processed by 'processEvent'
--
-- The 'Time.UTCTime' represents the time when the event was fetched from DB.
-- Used to calculate Event Lock time
type EventWithSource b = (Event, SourceConfig b, Time.UTCTime)

type FetchEventArguments = ([EventWithSource ('Postgres 'Vanilla)], Int , Bool)

-- | Service events from our in-DB queue.
--
-- There are a few competing concerns and constraints here; we want to...
--   - fetch events in batches for lower DB pressure
--   - don't fetch more than N at a time (since that can mean: space leak, less
--     effective scale out, possible double sends for events we've checked out
--     on exit (TODO clean shutdown procedure))
--   - try not to cause webhook workers to stall waiting on DB fetch
--   - limit webhook HTTP concurrency per HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE
processEventQueue
  :: forall m
   . ( HasVersion
     , MonadIO m
     , Tracing.HasReporter m
     , MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , MonadMask m
     )
  => L.Logger L.Hasura
  -> LogBehavior
  -> HTTP.Manager
  -> IO SchemaCache
  -> EventEngineCtx
  -> LockedEventsCtx
  -> ServerMetrics
  -> MaintenanceMode
  -> m (Forever m)
processEventQueue logger logBehavior httpMgr getSchemaCache EventEngineCtx{..} LockedEventsCtx{leEvents} serverMetrics maintenanceMode = do
  events0 <- popEventsBatch
  return $ Forever (events0, 0, False) go
  where
    fetchBatchSize = getNonNegativeInt _eeCtxFetchSize

    popEventsBatch :: m [EventWithSource ('Postgres 'Vanilla)]
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
      pgSources <- scSources <$> liftIO getSchemaCache
      liftIO . fmap concat $
        -- fetch pending events across all the sources asynchronously
        LA.forConcurrently (M.toList pgSources) \(sourceName, sourceCache) -> concat . toList <$>
          for (unsafeSourceTables @('Postgres 'Vanilla) sourceCache) \tables -> liftIO do
            -- count the number of event triggers on tables in this source
            let eventTriggerCount = sum (M.size . _tiEventTriggerInfoMap <$> tables)

            -- only process events for this source if at least one event trigger exists
            if eventTriggerCount > 0 then fmap (concat . toList) $
              for (unsafeSourceConfiguration @('Postgres 'Vanilla) sourceCache) \sourceConfig -> do
                fetchEventsTxE <-
                  case maintenanceMode of
                    MaintenanceModeEnabled -> do
                      maintenanceModeVersion <- runPgSourceReadTx sourceConfig getMaintenanceModeVersion
                      pure $ fmap (fetchEventsMaintenanceMode sourceName fetchBatchSize) maintenanceModeVersion
                    MaintenanceModeDisabled -> return $ Right $ fetchEvents sourceName fetchBatchSize
                liftIO $ do
                  case fetchEventsTxE of
                    Left err -> do
                      liftIO $ L.unLogger logger $ EventInternalErr err
                      return []
                    Right fetchEventsTx ->
                      runPgSourceWriteTx sourceConfig fetchEventsTx >>= \case
                        Left err -> do
                          liftIO $ L.unLogger logger $ EventInternalErr err
                          return []
                        Right events -> do
                          -- Track number of events fetched in EKG
                          _ <- liftIO $ EKG.Distribution.add (smNumEventsFetchedPerBatch serverMetrics) (fromIntegral $ length events)
                          -- The time when the events were fetched. This is used to calculate the average lock time of an event.
                          eventsFetchedTime <- liftIO getCurrentTime
                          saveLockedEvents (map eId events) leEvents
                          return $ map (, sourceConfig, eventsFetchedTime) events
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
          mask_ $ do
            liftIO $ atomically $ do  -- block until < HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE threads:
              capacity <- readTVar _eeCtxEventThreadsCapacity
              check $ capacity > 0
              writeTVar _eeCtxEventThreadsCapacity (capacity - 1)
            -- since there is some capacity in our worker threads, we can launch another:
            let restoreCapacity = liftIO $ atomically $
                  modifyTVar' _eeCtxEventThreadsCapacity (+ 1)
            t <- LA.async $ flip runReaderT (logger, httpMgr) $
                    processEvent eventWithSource `finally`
                      -- NOTE!: this needs to happen IN THE FORKED THREAD:
                      restoreCapacity
            LA.link t

        -- return when next batch ready; some 'processEvent' threads may be running.
        LA.wait eventsNextA

      let lenEvents = length events
      if | lenEvents == fetchBatchSize -> do
             -- If we've seen N fetches in a row from the DB come back full (i.e. only limited
             -- by our LIMIT clause), then we say we're clearly falling behind:
             let clearlyBehind = fullFetchCount >= 3
             unless alreadyWarned $
               when clearlyBehind $
                 L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                   "Events processor may not be keeping up with events generated in postgres, " <>
                   "or we're working on a backlog of events. Consider increasing " <>
                   "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
             return (eventsNext, (fullFetchCount+1), (alreadyWarned || clearlyBehind))
         | otherwise -> do
             when (lenEvents /= fetchBatchSize && alreadyWarned) $
               -- emit as warning in case users are only logging warning severity and saw above
               L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                 "It looks like the events processor is keeping up again."
             return (eventsNext, 0, False)

    processEvent
      :: forall io r
       . ( HasVersion
         , MonadIO io
         , MonadReader r io
         , Has HTTP.Manager r
         , Has (L.Logger L.Hasura) r
         , Tracing.HasReporter io
         , MonadMask io
         )
      => EventWithSource ('Postgres 'Vanilla)
      -> io ()
    processEvent (e, sourceConfig, eventFetchedTime) = do
      -- Track Queue Time of Event (in seconds). See `smEventQueueTime`
      -- Queue Time = Time when the event was fetched from DB - Time when the event is being processed
      eventProcessTime <- liftIO getCurrentTime
      let eventQueueTime = realToFrac $ diffUTCTime eventProcessTime eventFetchedTime
      _ <- liftIO $ EKG.Distribution.add (smEventQueueTime serverMetrics) eventQueueTime

      cache <- liftIO getSchemaCache

      tracingCtx <- liftIO (Tracing.extractEventContext (eEvent e))
      let spanName eti = "Event trigger: " <> unNonEmptyText (unTriggerName (etiName eti))
          runTraceT = maybe
            Tracing.runTraceT
            Tracing.runTraceTInContext
            tracingCtx

      maintenanceModeVersionEither :: Either QErr (Maybe MaintenanceModeVersion) <-
        case maintenanceMode of
          MaintenanceModeEnabled -> do
            maintenanceModeVersion <-
              liftIO $ runPgSourceReadTx sourceConfig getMaintenanceModeVersion
            return $ Just <$> maintenanceModeVersion
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
              liftIO (runPgSourceWriteTx sourceConfig $ do
                currentTime <- liftIO getCurrentTime
                -- For such an event, we unlock the event and retry after a minute
                setRetry e (addUTCTime 60 currentTime) maintenanceModeVersion)
              >>= flip onLeft logQErr
            Right eti -> runTraceT (spanName eti) do
              let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
                  retryConf = etiRetryConf eti
                  timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
                  responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
                  (headers, logHeaders) = prepareHeaders logBehavior (etiHeaders eti)
                  ep = createEventPayload retryConf e
                  payload = encode $ toJSON ep
                  extraLogCtx = ExtraLogContext (epId ep) (Just $ etiName eti)
                  requestDetails = RequestDetails $ LBS.length payload

              -- Track the number of active HTTP workers using EKG.
              res <- bracket_
                      (liftIO $ EKG.Gauge.inc $ smNumEventHTTPWorkers serverMetrics)
                      (liftIO $ EKG.Gauge.dec $ smNumEventHTTPWorkers serverMetrics)
                      (runExceptT $ tryWebhook headers responseTimeout payload webhook)
              logHTTPForET res extraLogCtx requestDetails logBehavior
              either
                (processError sourceConfig e retryConf logHeaders ep maintenanceModeVersion)
                (processSuccess sourceConfig e logHeaders ep maintenanceModeVersion) res
                >>= flip onLeft logQErr
      -- removing an event from the _eeCtxLockedEvents after the event has been processed:
      removeEventFromLockedEvents (eId e) leEvents

createEventPayload :: RetryConf -> Event ->  EventPayload
createEventPayload retryConf e = EventPayload
    { epId           = eId e
    , epTable        = QualifiedTableStrict { getQualifiedTable = eTable e}
    , epTrigger      = eTrigger e
    , epEvent        = eEvent e
    , epDeliveryInfo =  DeliveryInfo
      { diCurrentRetry = eTries e
      , diMaxRetries   = rcNumRetries retryConf
      }
    , epCreatedAt    = eCreatedAt e
    }

processSuccess
  :: ( MonadIO m )
  => SourceConfig ('Postgres 'Vanilla)
  -> Event
  -> [HeaderConf]
  -> EventPayload
  -> Maybe MaintenanceModeVersion
  -> HTTPResp a
  -> m (Either QErr ())
processSuccess sourceConfig e reqHeaders ep maintenanceModeVersion resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation ep respStatus reqHeaders respBody respHeaders
  liftIO $ runPgSourceWriteTx sourceConfig $ do
    insertInvocation invocation
    setSuccess e maintenanceModeVersion

processError
  :: ( MonadIO m )
  => SourceConfig ('Postgres 'Vanilla)
  -> Event
  -> RetryConf
  -> [HeaderConf]
  -> EventPayload
  -> Maybe MaintenanceModeVersion
  -> HTTPErr a
  -> m (Either QErr ())
processError sourceConfig e retryConf reqHeaders ep maintenanceModeVersion err = do
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ encode $ show excp
          mkInvocation ep 1000 reqHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvocation ep 1001 reqHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation ep respStatus reqHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvocation ep 500 reqHeaders errMsg []
  liftIO $ runPgSourceWriteTx sourceConfig $ do
    insertInvocation invocation
    retryOrSetError e retryConf maintenanceModeVersion err

retryOrSetError
  :: Event
  -> RetryConf
  -> Maybe MaintenanceModeVersion
  -> HTTPErr a
  -> Q.TxE QErr ()
retryOrSetError e retryConf maintenanceModeVersion err = do
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = mretryHeader >>= parseRetryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      setError e maintenanceModeVersion
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      setRetry e retryTime maintenanceModeVersion
  where
    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _              = Nothing

    parseRetryHeader = mfilter (> 0) . readMaybe . T.unpack

mkInvocation
  :: EventPayload -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation 'EventType
mkInvocation ep status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (epId ep)
      status
      (mkWebhookReq (toJSON ep) reqHeaders invocationVersionET)
      resp

logQErr :: ( MonadReader r m, Has (L.Logger L.Hasura) r, MonadIO m) => QErr -> m ()
logQErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLogger logger $ EventInternalErr err

getEventTriggerInfoFromEvent
  :: SchemaCache -> Event -> Either Text EventTriggerInfo
getEventTriggerInfoFromEvent sc e = do
  let table = eTable e
      mTableInfo = unsafeTableInfo @('Postgres 'Vanilla) (eSource e) table $ scSources sc
  tableInfo <- onNothing mTableInfo $ Left ("table '" <> table <<> "' not found")
  let triggerName = tmName $ eTrigger e
      mEventTriggerInfo = M.lookup triggerName (_tiEventTriggerInfoMap tableInfo)
  onNothing mEventTriggerInfo $ Left ("event trigger '" <> triggerNameToTxt triggerName
    <> "' on table '" <> table <<> "' not found")


---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: SourceName -> Int -> Q.TxE QErr [Event]
fetchEvents source limitI =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = NOW()
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    WHERE l.delivered = 'f' and l.error = 'f'
                          and (l.locked IS NULL or l.locked < (NOW() - interval '30 minute'))
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                          and l.archived = 'f'
                    /* NB: this ordering is important for our index `event_log_fetch_events` */
                    /* (see `init_pg_source.sql`) */
                    ORDER BY locked NULLS FIRST, next_retry_at NULLS FIRST, created_at
                    LIMIT $1
                    FOR UPDATE SKIP LOCKED )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |] (Identity limit) True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
          Event
          { eId        = id'
          , eSource    = source
          , eTable     = QualifiedObject sn tn
          , eTrigger   = TriggerMetadata trn
          , eEvent     = payload
          , eTries     = tries
          , eCreatedAt = created
          }
        limit = fromIntegral limitI :: Word64

fetchEventsMaintenanceMode :: SourceName -> Int -> MaintenanceModeVersion -> Q.TxE QErr [Event]
fetchEventsMaintenanceMode sourceName limitI = \case
  PreviousMMVersion ->
    map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.event_log
        SET locked = 't'
        WHERE id IN ( SELECT l.id
                      FROM hdb_catalog.event_log l
                      WHERE l.delivered = 'f' and l.error = 'f' and l.locked = 'f'
                            and (l.next_retry_at is NULL or l.next_retry_at <= now())
                            and l.archived = 'f'
                      ORDER BY created_at
                      LIMIT $1
                      FOR UPDATE SKIP LOCKED )
        RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
        |] (Identity limit) True
    where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
            Event
            { eId        = id'
            , eSource    = SNDefault  -- in v1, there'll only be the default source
            , eTable     = QualifiedObject sn tn
            , eTrigger   = TriggerMetadata trn
            , eEvent     = payload
            , eTries     = tries
            , eCreatedAt = created
            }
          limit = fromIntegral limitI :: Word64
  CurrentMMVersion -> fetchEvents sourceName limitI

insertInvocation :: Invocation 'EventType -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |] ( iEventId invo
             , toInt64 $ iStatus invo :: Int64
             , Q.AltJ $ toJSON $ iRequest invo
             , Q.AltJ $ toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log

          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

setSuccess :: Event -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setSuccess e = \case
  Just PreviousMMVersion ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.event_log
    SET delivered = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |] (Identity $ eId e) True
  Just CurrentMMVersion -> latestVersionSetSuccess
  Nothing               -> latestVersionSetSuccess
  where
    latestVersionSetSuccess =
      Q.unitQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET delivered = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |] (Identity $ eId e) True

setError :: Event -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setError e = \case
  Just PreviousMMVersion ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.event_log
    SET error = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |] (Identity $ eId e) True
  Just CurrentMMVersion -> latestVersionSetError
  Nothing                -> latestVersionSetError
  where
    latestVersionSetError =
      Q.unitQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET error = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |] (Identity $ eId e) True

setRetry :: Event -> UTCTime -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setRetry e time = \case
  Just PreviousMMVersion ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.event_log
    SET next_retry_at = $1, locked = 'f'
    WHERE id = $2
    |] (time, eId e) True
  Just CurrentMMVersion -> latestVersionSetRetry
  Nothing                -> latestVersionSetRetry
  where
    latestVersionSetRetry =
      Q.unitQE defaultTxErrorHandler [Q.sql|
              UPDATE hdb_catalog.event_log
              SET next_retry_at = $1, locked = NULL
              WHERE id = $2
              |] (time, eId e) True

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral

-- | unlockEvents takes an array of 'EventId' and unlocks them. This function is called
--   when a graceful shutdown is initiated.
unlockEvents :: [EventId] -> Q.TxE QErr Int
unlockEvents eventIds =
   runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
   [Q.sql|
     WITH "cte" AS
     (UPDATE hdb_catalog.event_log
     SET locked = NULL
     WHERE id = ANY($1::text[])
     -- only unlock those events that have been locked, it's possible
     -- that an event has been processed but not yet been removed from
     -- the saved locked events, which will lead to a double send
     AND locked IS NOT NULL
     RETURNING *)
     SELECT count(*) FROM "cte"
   |] (Identity $ PGTextArray $ map unEventId eventIds) True

getMaintenanceModeVersion :: Q.TxE QErr MaintenanceModeVersion
getMaintenanceModeVersion = liftTx $ do
  catalogVersion <- getCatalogVersion -- From the user's DB
  -- the previous version and the current version will change depending
  -- upon between which versions we need to support maintenance mode
  if | catalogVersion == "40"     -> pure PreviousMMVersion
     -- The catalog is migrated to the 43rd version for a source
     -- which was initialised by a v1 graphql-engine instance (See @initSource@).
     | catalogVersion == "43"     -> pure CurrentMMVersion
     | catalogVersion == latestCatalogVersionString -> pure CurrentMMVersion
     | otherwise                  ->
       throw500 $
         "Maintenance mode is only supported with catalog versions: 40, 43 and "
         <> tshow latestCatalogVersionString
