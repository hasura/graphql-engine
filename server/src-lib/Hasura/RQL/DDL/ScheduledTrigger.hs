module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , runUpdateScheduledTrigger
  , runDeleteScheduledTrigger
  , runCancelScheduledEvent
  , addScheduledTriggerToCatalog
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

runCreateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTriggerWith ->  m EncJSON
runCreateScheduledTrigger (CreateScheduledTriggerWith definition includeInMetadata) = do
  addScheduledTriggerToCatalog definition includeInMetadata
  buildSchemaCacheFor $ MOScheduledTrigger $ stName definition
  return successMsg

addScheduledTriggerToCatalog :: (MonadTx m) => CreateScheduledTrigger -> Bool ->  m ()
addScheduledTriggerToCatalog CreateScheduledTrigger {..} includeInMetadata = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_scheduled_trigger
        (name, webhook_conf, schedule_conf, payload, retry_conf, include_in_metadata)
      VALUES ($1, $2, $3, $4, $5, $6)
    |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
       , includeInMetadata) False
  case stSchedule of
    OneOff timestamp -> Q.unitQE defaultTxErrorHandler
      [Q.sql|
        INSERT into hdb_catalog.hdb_scheduled_events
          (name, scheduled_time)
         VALUES ($1, $2)
      |] (stName, timestamp) False
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

runUpdateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTriggerWith -> m EncJSON
runUpdateScheduledTrigger (CreateScheduledTriggerWith definition includeInMetadata) = do
  updateScheduledTriggerInCatalog definition includeInMetadata
  buildSchemaCacheFor $ MOScheduledTrigger $ stName definition
  return successMsg

updateScheduledTriggerInCatalog :: (MonadTx m) => CreateScheduledTrigger -> Bool -> m ()
updateScheduledTriggerInCatalog CreateScheduledTrigger {..} includeInMetadata = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_scheduled_trigger
    SET webhook_conf = $2,
        schedule_conf = $3,
        payload = $4,
        retry_conf = $5,
        include_in_metadata = $6
    WHERE name = $1
   |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
      , includeInMetadata) False
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
