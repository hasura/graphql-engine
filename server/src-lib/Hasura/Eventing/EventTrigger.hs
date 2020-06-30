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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Hasura.Eventing.EventTrigger
  ( initEventEngineCtx
  , processEventQueue
  , unlockAllEvents
  , defaultMaxEventThreads
  , defaultFetchInterval
  , Event(..)
  , unlockEvents
  , EventEngineCtx(..)
  ) where


import           Control.Concurrent.Async    (wait, withAsync)
import           Control.Concurrent.Extended (sleep)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch         (MonadMask, bracket_)
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Has
import           Data.Int                    (Int64)
import           Data.String
import           Data.Time.Clock
import           Data.Word
import           Hasura.Eventing.HTTP
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Version       (HasVersion)
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict           as M
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Database.PG.Query             as Q
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Client           as HTTP
import qualified Database.PG.Query.PTI         as PTI
import qualified PostgreSQL.Binary.Encoding    as PE
import qualified Data.Set                      as Set

data TriggerMetadata
  = TriggerMetadata { tmName :: TriggerName }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerMetadata)

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr L.Hasura where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, L.eventTriggerLogType, toJSON qerr)

-- | Change data for a particular row
--
-- https://docs.hasura.io/1.0/graphql/manual/event-triggers/payload.html
data Event
  = Event
  { eId        :: EventId
  , eTable     :: QualifiedTable
  , eTrigger   :: TriggerMetadata
  , eEvent     :: Value
  , eTries     :: Int
  , eCreatedAt :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveFromJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''Event)

data EventEngineCtx
  = EventEngineCtx
  { _eeCtxEventThreadsCapacity :: TVar Int
  , _eeCtxFetchInterval        :: DiffTime
  , _eeCtxLockedEvents         :: TVar (Set.Set EventId)
  }

data DeliveryInfo
  = DeliveryInfo
  { diCurrentRetry :: Int
  , diMaxRetries   :: Int
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DeliveryInfo)

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

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventPayload)

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultFetchInterval :: DiffTime
defaultFetchInterval = seconds 1

initEventEngineCtx :: Int -> DiffTime -> STM EventEngineCtx
initEventEngineCtx maxT _eeCtxFetchInterval = do
  _eeCtxEventThreadsCapacity <- newTVar maxT
  _eeCtxLockedEvents <- newTVar Set.empty
  return $ EventEngineCtx{..}

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
  :: (HasVersion) => L.Logger L.Hasura -> LogEnvHeaders -> HTTP.Manager-> Q.PGPool
  -> IO SchemaCache -> EventEngineCtx
  -> IO void
processEventQueue logger logenv httpMgr pool getSchemaCache eeCtx@EventEngineCtx{..} = do
  events0 <- popEventsBatch
  go events0 0 False
  where
    hasEventTrigger = not . null . _tiEventTriggerInfoMap
    fetchBatchSize = 100
    popEventsBatch = do
      cache <- liftIO getSchemaCache
      let tables = scTables cache
      -- only fetch events from db if there is atleast one event trigger
      if | any hasEventTrigger tables -> do
             let run = runExceptT . Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite)
             run (fetchEvents fetchBatchSize) >>= \case
                 Left err -> do
                   L.unLogger logger $ EventInternalErr err
                   return []
                 Right events -> do
                   saveLockedEvents events
                   return events
         | otherwise -> do
           L.unLogger logger $ L.debugT "no event triggers found, skipping fetch"
           return []

    -- After the events are fetched from the DB, we store the locked events
    -- in a hash set(order doesn't matter and look ups are faster) in the
    -- event engine context
    saveLockedEvents :: [Event] -> IO ()
    saveLockedEvents evts =
      liftIO $ atomically $ do
        lockedEvents <- readTVar _eeCtxLockedEvents
        let evtsIds = map eId evts
        let newLockedEvents = Set.union lockedEvents (Set.fromList evtsIds)
        writeTVar _eeCtxLockedEvents $! newLockedEvents

    removeEventFromLockedEvents :: EventId -> IO ()
    removeEventFromLockedEvents eventId = do
      liftIO $ atomically $ do
        lockedEvents <- readTVar _eeCtxLockedEvents
        writeTVar _eeCtxLockedEvents $! Set.delete eventId lockedEvents

    -- work on this batch of events while prefetching the next. Recurse after we've forked workers
    -- for each in the batch, minding the requested pool size.
    go :: [Event] -> Int -> Bool -> IO void
    go events !fullFetchCount !alreadyWarned = do
      -- process events ASAP until we've caught up; only then can we sleep
      when (null events) $ sleep _eeCtxFetchInterval

      -- Prefetch next events payload while concurrently working through our current batch.
      -- NOTE: we probably don't need to prefetch so early, but probably not
      -- worth the effort for something more fine-tuned
      eventsNext <- withAsync popEventsBatch $ \eventsNextA -> do
        -- process approximately in order, minding HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE:
        forM_ events $ \event -> do
             runReaderT (withEventEngineCtx eeCtx $ (processEvent event)) (logger, httpMgr)
             -- removing an event from the _eeCtxLockedEvents after the event has
             -- been processed
             removeEventFromLockedEvents (eId event)
        wait eventsNextA

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
             go eventsNext (fullFetchCount+1) (alreadyWarned || clearlyBehind)

         | otherwise -> do
             when (lenEvents /= fetchBatchSize && alreadyWarned) $
               -- emit as warning in case users are only logging warning severity and saw above
               L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                 "It looks like the events processor is keeping up again."
             go eventsNext 0 False

    processEvent
      :: ( HasVersion
         , MonadReader r m
         , Has HTTP.Manager r
         , Has (L.Logger L.Hasura) r
         , MonadIO m
         )
      => Event -> m ()
    processEvent e = do
      cache <- liftIO getSchemaCache
      let meti = getEventTriggerInfoFromEvent cache e
      case meti of
        Nothing -> do
          --  This rare error can happen in the following known cases:
          --  i) schema cache is not up-to-date (due to some bug, say during schema syncing across multiple instances)
          --  ii) the event trigger is dropped when this event was just fetched
          logQErr $ err500 Unexpected "table or event-trigger not found in schema cache"
          liftIO . runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
            currentTime <- liftIO getCurrentTime
            -- For such an event, we unlock the event and retry after a minute
            setRetry e (addUTCTime 60 currentTime)
          >>= flip onLeft logQErr
        Just eti -> do
          let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
              retryConf = etiRetryConf eti
              timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
              responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
              headerInfos = etiHeaders eti
              etHeaders = map encodeHeader headerInfos
              headers = addDefaultHeaders etHeaders
              ep = createEventPayload retryConf e
              extraLogCtx = ExtraLogContext Nothing (epId ep) -- avoiding getting current time here to avoid another IO call with each event call
          res <- runExceptT $ tryWebhook headers responseTimeout (toJSON ep) webhook
          logHTTPForET res extraLogCtx
          let decodedHeaders = map (decodeHeader logenv headerInfos) headers
          either
            (processError pool e retryConf decodedHeaders ep)
            (processSuccess pool e decodedHeaders ep) res
            >>= flip onLeft logQErr

withEventEngineCtx ::
    ( MonadIO m
    , MonadMask m
    )
    => EventEngineCtx -> m () -> m ()
withEventEngineCtx eeCtx = bracket_ (decrementThreadCount eeCtx) (incrementThreadCount eeCtx)

incrementThreadCount :: MonadIO m => EventEngineCtx -> m ()
incrementThreadCount (EventEngineCtx c _ _) = liftIO $ atomically $ modifyTVar' c (+1)

decrementThreadCount :: MonadIO m => EventEngineCtx -> m ()
decrementThreadCount (EventEngineCtx c _ _) = liftIO $ atomically $ do
  countThreads <- readTVar c
  if countThreads > 0
     then modifyTVar' c (\v -> v - 1)
     else retry

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
  => Q.PGPool -> Event -> [HeaderConf] -> EventPayload -> HTTPResp a
  -> m (Either QErr ())
processSuccess pool e decodedHeaders ep resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation ep respStatus decodedHeaders respBody respHeaders
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    setSuccess e

processError
  :: ( MonadIO m )
  => Q.PGPool -> Event -> RetryConf -> [HeaderConf] -> EventPayload -> HTTPErr a
  -> m (Either QErr ())
processError pool e retryConf decodedHeaders ep err = do
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ encode $ show excp
          mkInvocation ep 1000 decodedHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvocation ep 1001 decodedHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation ep respStatus decodedHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = (TBS.fromLBS $ encode detail)
          mkInvocation ep 500 decodedHeaders errMsg []
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    retryOrSetError e retryConf err

retryOrSetError :: Event -> RetryConf -> HTTPErr a -> Q.TxE QErr ()
retryOrSetError e retryConf err = do
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = mretryHeader >>= parseRetryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      setError e
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      setRetry e retryTime
  where
    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _              = Nothing

    parseRetryHeader = mfilter (> 0) . readMaybe . T.unpack

mkInvocation
  :: EventPayload -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> (Invocation 'EventType)
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

getEventTriggerInfoFromEvent :: SchemaCache -> Event -> Maybe EventTriggerInfo
getEventTriggerInfoFromEvent sc e = let table = eTable e
                                        tableInfo = M.lookup table $ scTables sc
                                    in M.lookup ( tmName $ eTrigger e) =<< (_tiEventTriggerInfoMap <$> tableInfo)

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: Int -> Q.TxE QErr [Event]
fetchEvents limitI =
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
          , eTable     = QualifiedObject sn tn
          , eTrigger   = TriggerMetadata trn
          , eEvent     = payload
          , eTries     = tries
          , eCreatedAt = created
          }
        limit = fromIntegral limitI :: Word64

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

setSuccess :: Event -> Q.TxE QErr ()
setSuccess e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET delivered = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setError :: Event -> Q.TxE QErr ()
setError e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET error = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setRetry :: Event -> UTCTime -> Q.TxE QErr ()
setRetry e time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = $1, locked = 'f'
          WHERE id = $2
          |] (time, eId e) True

unlockAllEvents :: Q.TxE QErr ()
unlockAllEvents =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 'f'
          WHERE locked = 't'
          |] () True

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral

-- EventIdArray is only used for PG array encoding
newtype EventIdArray = EventIdArray { unEventIdArray :: [EventId]} deriving (Show, Eq)

instance Q.ToPrepArg EventIdArray where
  toPrepVal (EventIdArray l) = Q.toPrepValHelper PTI.unknown encoder $ l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)

-- | unlockEvents takes an array of 'EventId' and unlocks them. This function is called
--   when a graceful shutdown is initiated.
unlockEvents :: [EventId] -> Q.TxE QErr Int
unlockEvents eventIds =
   (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
   [Q.sql|
     WITH "cte" AS
     (UPDATE hdb_catalog.event_log
     SET locked = 'f'
     WHERE id = ANY($1::text[])
     -- only unlock those events that have been locked, it's possible
     -- that an event has been processed but not yet been removed from
     -- the saved locked events, which will lead to a double send
     AND locked = 't'
     RETURNING *)
     SELECT count(*) FROM "cte"
   |] (Identity $ EventIdArray eventIds) True
