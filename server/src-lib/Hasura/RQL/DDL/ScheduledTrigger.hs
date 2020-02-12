module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , runUpdateScheduledTrigger
  , runDeleteScheduledTrigger
  , runCancelScheduledEvent
  , resolveScheduledTrigger
  , deleteScheduledTriggerFromCatalog
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger ( getWebhookInfoFromConf
                                             , getHeaderInfosFromConf)
import           Hasura.RQL.Types

import qualified Database.PG.Query     as Q

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
        (name, webhook_conf, schedule, payload, retry_conf)
      VALUES ($1, $2, $3, $4, $5)
    |] (stName, Q.AltJ stWebhookConf, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf) False
  case stSchedule of
    Cron _ -> pure ()
    OneOff timestamp -> Q.unitQE defaultTxErrorHandler
      [Q.sql|
        INSERT into hdb_catalog.hdb_scheduled_events
          (name, scheduled_time)
         VALUES ($1, $2)
      |] (stName, timestamp) False

resolveScheduledTrigger
  :: (QErrM m, MonadIO m)
  => CreateScheduledTrigger -> m ScheduledTriggerInfo
resolveScheduledTrigger CreateScheduledTrigger {..} = do
  webhookInfo <- getWebhookInfoFromConf stWebhookConf
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

runUpdateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTrigger ->  m EncJSON
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
        schedule = $3,
        payload = $4,
        retry_conf = $5
    WHERE name = $1
   |] (stName, Q.AltJ stWebhookConf, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf) False
  -- since the scheduled trigger is updated, clear all its future events which are not retries
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_events
    WHERE name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity stName) False

runDeleteScheduledTrigger :: (CacheRWM m, MonadTx m) => TriggerName -> m EncJSON
runDeleteScheduledTrigger stName = do
  deleteScheduledTriggerFromCatalog stName
  return successMsg

deleteScheduledTriggerFromCatalog :: (MonadTx m) => TriggerName -> m ()
deleteScheduledTriggerFromCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_trigger
    WHERE name = $1
   |] (Identity stName) False

runCancelScheduledEvent :: (MonadTx m) => EventId -> m EncJSON
runCancelScheduledEvent se = do
  affectedRows <- deleteScheduledEventFromCatalog se
  if | affectedRows == 1 -> pure successMsg
     | affectedRows == 0 -> throw400 NotFound "scheduled event not found"
     | otherwise -> throw500 "unexpected: more than one scheduled events cancelled"

deleteScheduledEventFromCatalog :: (MonadTx m) => EventId -> m Int
deleteScheduledEventFromCatalog seId = liftTx $ do
  (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_events
    WHERE id = $1
    RETURNING count(*)
   |] (Identity seId) False
