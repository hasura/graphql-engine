module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , runUpdateScheduledTrigger
  , runDeleteScheduledTrigger
  , runCreateScheduledEvent
  , runCancelScheduledEvent
  , runTrackScheduledTrigger
  , runUntrackScheduledTrigger
  , addScheduledTriggerToCatalog
  , deleteScheduledTriggerFromCatalog
  , trackScheduledTriggerInCatalog
  , resolveScheduledTrigger
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger ( getWebhookInfoFromConf
                                             , getHeaderInfosFromConf)
import           Hasura.RQL.Types

import           Hasura.Eventing.ScheduledTrigger

import qualified Database.PG.Query     as Q
import qualified Data.Time.Clock       as C

runCreateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTrigger ->  m EncJSON
runCreateScheduledTrigger q = do
  addScheduledTriggerToCatalog q
  buildSchemaCacheFor $ MOScheduledTrigger $ stName q
  return successMsg

addScheduledTriggerToCatalog :: (MonadTx m) => CreateScheduledTrigger ->  m ()
addScheduledTriggerToCatalog CreateScheduledTrigger {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_scheduled_trigger
        (name, webhook_conf, schedule_conf, payload, retry_conf)
      VALUES ($1, $2, $3, $4, $5)
    |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
       ) False
  case stSchedule of
    AdHoc (Just timestamp) -> Q.unitQE defaultTxErrorHandler
      [Q.sql|
        INSERT into hdb_catalog.hdb_scheduled_events
          (name, scheduled_time)
         VALUES ($1, $2)
      |] (stName, timestamp) False
    Cron cron -> do
      currentTime <- liftIO C.getCurrentTime
      let scheduleTimes = generateScheduleTimes currentTime 100 cron -- generate next 100 events
          events = map (ScheduledEventSeed stName) scheduleTimes
      insertScheduledEvents events
    _ -> pure ()

resolveScheduledTrigger
  :: (QErrM m, MonadIO m)
  => CreateScheduledTrigger -> m ScheduledTriggerInfo
resolveScheduledTrigger CreateScheduledTrigger {..} = do
  webhookInfo <- getWebhookInfoFromConf stWebhook
  headerInfo <- getHeaderInfosFromConf stHeaders
  let stInfo =
        ScheduledTriggerInfo
          stName
          stSchedule
          stPayload
          stRetryConf
          webhookInfo
          headerInfo
  pure stInfo

runUpdateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTrigger -> m EncJSON
runUpdateScheduledTrigger q = do
  updateScheduledTriggerInCatalog q
  buildSchemaCacheFor $ MOScheduledTrigger $ stName q
  return successMsg

updateScheduledTriggerInCatalog :: (MonadTx m) => CreateScheduledTrigger -> m ()
updateScheduledTriggerInCatalog CreateScheduledTrigger {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_scheduled_trigger
    SET webhook_conf = $2,
        schedule_conf = $3,
        payload = $4,
        retry_conf = $5
    WHERE name = $1
   |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
      ) False
  -- since the scheduled trigger is updated, clear all its future events which are not retries
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_events
    WHERE name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity stName) False

runDeleteScheduledTrigger :: (CacheRWM m, MonadTx m) => ScheduledTriggerName -> m EncJSON
runDeleteScheduledTrigger (ScheduledTriggerName stName) = do
  deleteScheduledTriggerFromCatalog stName
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

deleteScheduledTriggerFromCatalog :: (MonadTx m) => TriggerName -> m ()
deleteScheduledTriggerFromCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_trigger
    WHERE name = $1
   |] (Identity stName) False

runCreateScheduledEvent :: (MonadTx m) => CreateScheduledEvent -> m EncJSON
runCreateScheduledEvent CreateScheduledEvent{..} = do
  liftTx $ Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_scheduled_events
        (name, scheduled_time, additional_payload)
       VALUES ($1, $2, $3)
    |] (steName, steTimestamp, Q.AltJ <$> stePayload) False
  pure successMsg

runCancelScheduledEvent :: (MonadTx m) => ScheduledEventId -> m EncJSON
runCancelScheduledEvent (ScheduledEventId seId) = do
  affectedRows <- deleteScheduledEventFromCatalog seId
  if | affectedRows == 1 -> pure successMsg
     | affectedRows == 0 -> throw400 NotFound "scheduled event not found"
     | otherwise -> throw500 "unexpected: more than one scheduled events cancelled"

deleteScheduledEventFromCatalog :: (MonadTx m) => EventId -> m Int
deleteScheduledEventFromCatalog seId = liftTx $ do
  (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
   [Q.sql|
    WITH "cte" AS
    (UPDATE hdb_catalog.hdb_scheduled_events SET cancelled = 't' WHERE id = $1 RETURNING *)
    SELECT count(*) FROM "cte"
   |] (Identity seId) False

runTrackScheduledTrigger :: (MonadTx m) => ScheduledTriggerName -> m EncJSON
runTrackScheduledTrigger (ScheduledTriggerName stName) = do
  trackScheduledTriggerInCatalog stName
  return successMsg

trackScheduledTriggerInCatalog :: (MonadTx m) => TriggerName -> m ()
trackScheduledTriggerInCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_scheduled_trigger
    SET include_in_metadata = 't'
    WHERE name = $1
   |] (Identity stName) False

runUntrackScheduledTrigger :: (MonadTx m) => ScheduledTriggerName -> m EncJSON
runUntrackScheduledTrigger (ScheduledTriggerName stName) = do
  untrackScheduledTriggerInCatalog stName
  return successMsg

untrackScheduledTriggerInCatalog :: (MonadTx m) => TriggerName -> m ()
untrackScheduledTriggerInCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_scheduled_trigger
    SET include_in_metadata = 'f'
    WHERE name = $1
   |] (Identity stName) False
