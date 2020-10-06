module Hasura.Class
  ( SchemaSyncEventProcessResult(..)
  , SchemaSyncEventPayload(..)
  , MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Hasura.Db
import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.GraphQL.Parser                  hiding (column)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Session
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select.Internal         as RS

import           Control.Monad.Morph                    (MFunctor, hoist)
import           Data.Int                               (Int64)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Time                              as UTC
import qualified Database.PG.Query                      as Q
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Text.Builder                           as TB (run)

data SchemaSyncEventProcessResult
  = SchemaSyncEventProcessResult
  { _sseprShouldReload       :: !Bool
  , _sseprCacheInvalidations :: !CacheInvalidations
  }

data SchemaSyncEventPayload
  = SchemaSyncEventPayload
  { _ssepInstanceId    :: !InstanceId
  , _ssepOccurredAt    :: !UTC.UTCTime
  , _ssepInvalidations :: !CacheInvalidations
  }
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''SchemaSyncEventPayload)

newtype MetadataStorageT m a
  = MetadataStorageT {unMetadataStorageT :: ExceptT QErr m a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MFunctor
           , MonadTrans
           , MonadIO
           )

runMetadataStorageT
  :: MetadataStorageT m a -> m (Either QErr a)
runMetadataStorageT =
  runExceptT . unMetadataStorageT

-- Related to Async actions
type AsyncActionQueryResolver = UserInfo -> AnnActionAsyncQuery UnpreparedValue -> RS.AnnSimpleSelG UnpreparedValue

-- getDeprivedCronTriggerStats :: DeprivedCronTriggersStatsTx
-- getDeprivedCronTriggerStats =
--   map (\(n, count, maxTx) -> CronTriggerStats n count maxTx) <$>
--     Q.listQE defaultTxErrorHandler
--     [Q.sql|
--      SELECT name, upcoming_events_count, max_scheduled_time
--       FROM hdb_catalog.hdb_cron_events_stats
--       WHERE upcoming_events_count < 100
--      |] () True

-- getPartialCronEvents :: PartialCronEventsTx
-- getPartialCronEvents = do
--   map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
--       UPDATE hdb_catalog.hdb_cron_events
--       SET status = 'locked'
--       WHERE id IN ( SELECT t.id
--                     FROM hdb_catalog.hdb_cron_events t
--                     WHERE ( t.status = 'scheduled'
--                             and (
--                              (t.next_retry_at is NULL and t.scheduled_time <= now()) or
--                              (t.next_retry_at is not NULL and t.next_retry_at <= now())
--                             )
--                           )
--                     FOR UPDATE SKIP LOCKED
--                     )
--       RETURNING id, trigger_name, scheduled_time, tries, created_at
--       |] () True
--   where uncurryEvent (i, n, st, tries, createdAt) = CronEventPartial i n st tries createdAt

-- getOneOffScheduledEvents :: OneOffScheduledEventsTx
-- getOneOffScheduledEvents = do
--   map uncurryOneOffScheduledEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
--       UPDATE hdb_catalog.hdb_scheduled_events
--       SET status = 'locked'
--       WHERE id IN ( SELECT t.id
--                     FROM hdb_catalog.hdb_scheduled_events t
--                     WHERE ( t.status = 'scheduled'
--                             and (
--                              (t.next_retry_at is NULL and t.scheduled_time <= now()) or
--                              (t.next_retry_at is not NULL and t.next_retry_at <= now())
--                             )
--                           )
--                     FOR UPDATE SKIP LOCKED
--                     )
--       RETURNING id, webhook_conf, scheduled_time, retry_conf, payload, header_conf, tries, comment, created_at
--       |] () False
--   where
--     uncurryOneOffScheduledEvent ( eventId
--                                 , webhookConf
--                                 , scheduledTime
--                                 , retryConf
--                                 , payload
--                                 , headerConf
--                                 , tries
--                                 , comment
--                                 , createdAt) =
--       OneOffScheduledEvent eventId
--                            scheduledTime
--                            tries
--                            (Q.getAltJ webhookConf)
--                            (Q.getAltJ payload)
--                            (Q.getAltJ retryConf)
--                            (Q.getAltJ headerConf)
--                            comment
--                            createdAt

-- insertCronEvents :: InsertCronEventsTx
-- insertCronEvents events = do
--   let insertCronEventsSql = TB.run $ toSQL
--         SQLInsert
--           { siTable    = cronEventsTable
--           , siCols     = map unsafePGCol ["trigger_name", "scheduled_time"]
--           , siValues   = ValuesExp $ map (toTupleExp . toArr) events
--           , siConflict = Just $ DoNothing Nothing
--           , siRet      = Nothing
--           }
--   Q.unitQE defaultTxErrorHandler (Q.fromText insertCronEventsSql) () False
--   where
--     toArr (CronEventSeed n t) = [(triggerNameToTxt n), (formatTime' t)]
--     toTupleExp = TupleExp . map SELit

-- insertInvocation :: InsertInvocationTx
-- insertInvocation invo type' = do
--   case type' of
--     Cron -> do
--       Q.unitQE defaultTxErrorHandler
--         [Q.sql|
--          INSERT INTO hdb_catalog.hdb_cron_event_invocation_logs
--          (event_id, status, request, response)
--          VALUES ($1, $2, $3, $4)
--         |] ( iEventId invo
--              , fromIntegral $ iStatus invo :: Int64
--              , Q.AltJ $ J.toJSON $ iRequest invo
--              , Q.AltJ $ J.toJSON $ iResponse invo) True
--       Q.unitQE defaultTxErrorHandler [Q.sql|
--           UPDATE hdb_catalog.hdb_cron_events
--           SET tries = tries + 1
--           WHERE id = $1
--           |] (Identity $ iEventId invo) True
--     OneOff -> do
--       Q.unitQE defaultTxErrorHandler
--         [Q.sql|
--          INSERT INTO hdb_catalog.hdb_scheduled_event_invocation_logs
--          (event_id, status, request, response)
--          VALUES ($1, $2, $3, $4)
--         |] ( iEventId invo
--              , fromIntegral $ iStatus invo :: Int64
--              , Q.AltJ $ J.toJSON $ iRequest invo
--              , Q.AltJ $ J.toJSON $ iResponse invo) True
--       Q.unitQE defaultTxErrorHandler [Q.sql|
--           UPDATE hdb_catalog.hdb_scheduled_events
--           SET tries = tries + 1
--           WHERE id = $1
--           |] (Identity $ iEventId invo) True

-- setRetry :: SetRetryTx
-- setRetry se time type' =
--   case type' of
--     Cron ->
--       Q.unitQE defaultTxErrorHandler [Q.sql|
--         UPDATE hdb_catalog.hdb_cron_events
--         SET next_retry_at = $1,
--         STATUS = 'scheduled'
--         WHERE id = $2
--         |] (time, sefId se) True
--     OneOff ->
--       Q.unitQE defaultTxErrorHandler [Q.sql|
--         UPDATE hdb_catalog.hdb_scheduled_events
--         SET next_retry_at = $1,
--         STATUS = 'scheduled'
--         WHERE id = $2
--         |] (time, sefId se) True

-- setScheduledEventStatus :: SetScheduledEventStatusTx
-- setScheduledEventStatus scheduledEventId status type' =
--   case type' of
--     Cron -> do
--       Q.unitQE defaultTxErrorHandler
--        [Q.sql|
--         UPDATE hdb_catalog.hdb_cron_events
--         SET status = $2
--         WHERE id = $1
--        |] (scheduledEventId, status) True
--     OneOff -> do
--       Q.unitQE defaultTxErrorHandler
--        [Q.sql|
--         UPDATE hdb_catalog.hdb_scheduled_events
--         SET status = $2
--         WHERE id = $1
--        |] (scheduledEventId, status) True

-- unlockScheduledEvents :: UnlockScheduledEventsTx
-- unlockScheduledEvents type' eventIds =
--   case type' of
--     Cron ->
--       (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
--       [Q.sql|
--         WITH "cte" AS
--         (UPDATE hdb_catalog.hdb_cron_events
--         SET status = 'scheduled'
--         WHERE id = ANY($1::text[]) and status = 'locked'
--         RETURNING *)
--         SELECT count(*) FROM "cte"
--       |] (Identity $ ScheduledEventIdArray eventIds) True

--     OneOff ->
--       (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
--       [Q.sql|
--         WITH "cte" AS
--         (UPDATE hdb_catalog.hdb_scheduled_events
--         SET status = 'scheduled'
--         WHERE id = ANY($1::text[]) AND status = 'locked'
--         RETURNING *)
--         SELECT count(*) FROM "cte"
--       |] (Identity $ ScheduledEventIdArray eventIds) True

-- unlockAllLockedScheduledEvents :: UnlockAllLockedScheduledEventsTx
-- unlockAllLockedScheduledEvents = do
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--           UPDATE hdb_catalog.hdb_cron_events
--           SET status = 'scheduled'
--           WHERE status = 'locked'
--           |] () True
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--           UPDATE hdb_catalog.hdb_scheduled_events
--           SET status = 'scheduled'
--           WHERE status = 'locked'
--           |] () True

-- clearFutureCronEvents :: ClearFutureCronEventsTx
-- clearFutureCronEvents triggerName =
--   Q.unitQE defaultTxErrorHandler
--   [Q.sql|
--     DELETE FROM hdb_catalog.hdb_cron_events
--     WHERE trigger_name = $1 AND scheduled_time > now() AND tries = 0
--   |] (Identity triggerName) False


class (Monad m) => MonadMetadataStorage m where

  -- Metadata
  getMetadata :: MetadataStorageT m Metadata

  setMetadata :: Metadata -> MetadataStorageT m ()
  notifySchemaCacheSync :: InstanceId -> CacheInvalidations -> MetadataStorageT m ()
  processSchemaSyncEventPayload
    :: InstanceId -> J.Value -> MetadataStorageT m SchemaSyncEventProcessResult

  -- Async actions
  insertAction
    :: ActionName -> SessionVariables -> [HTTP.Header] -> J.Value
    -> MetadataStorageT m ActionId
  fetchUndeliveredActionEvents :: MetadataStorageT m [ActionLogItem]
  setActionStatus :: ActionId -> AsyncActionStatus -> MetadataStorageT m ()
  fetchActionResponse :: ActionId -> MetadataStorageT m ActionLogResponse

  -- Scheduled triggers
  getDeprivedCronTriggerStats :: MetadataStorageT m [CronTriggerStats]
  getPartialCronEvents :: MetadataStorageT m [CronEventPartial]
  getOneOffScheduledEvents :: MetadataStorageT m [OneOffScheduledEvent]
  insertCronEvents :: [CronEventSeed] -> MetadataStorageT m ()
  insertScheduledEventInvocation
    :: Invocation 'ScheduledType -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventRetry
    :: ScheduledEventFull -> UTC.UTCTime -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventStatus
    :: ScheduledEventId -> ScheduledEventStatus -> ScheduledEventType -> MetadataStorageT m ()
  unlockScheduledEvents
    :: ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT m Int
  unlockAllLockedScheduledEvents :: MetadataStorageT m ()
  clearFutureCronEvents :: TriggerName -> MetadataStorageT m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents
