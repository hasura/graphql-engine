module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , runUpdateScheduledTrigger
  , runDeleteScheduledTrigger
  , runCancelScheduledEvent
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache       (CacheBuildM)
import           Hasura.RQL.DDL.EventTrigger ( getWebhookInfoFromConf
                                             , getHeaderInfosFromConf)
import           Hasura.RQL.Types.Helpers
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger (TriggerName)
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.SchemaCache ( addScheduledTriggerToCache
                                              , updateScheduledTriggerInCache
                                              , removeScheduledTriggerFromCache
                                              , ScheduledTriggerInfo(..))

import qualified Database.PG.Query     as Q

runCreateScheduledTrigger :: CacheBuildM m => CreateScheduledTrigger ->  m EncJSON
runCreateScheduledTrigger q = do
  sti <- scheduledTriggerSetup q
  addScheduledTriggerToCache sti
  addScheduledTriggerToCatalog q
  return successMsg

addScheduledTriggerToCatalog :: CacheBuildM m => CreateScheduledTrigger ->  m ()
addScheduledTriggerToCatalog CreateScheduledTrigger {..} = liftTx $
  Q.unitQE defaultTxErrorHandler
  [Q.sql|
    INSERT into hdb_catalog.hdb_scheduled_trigger
                (name, webhook_conf, schedule, payload, retry_conf)
    VALUES ($1, $2, $3, $4, $5)
  |] (stName, Q.AltJ stWebhookConf, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf) False

scheduledTriggerSetup ::
     (CacheBuildM m) => CreateScheduledTrigger -> m ScheduledTriggerInfo
scheduledTriggerSetup CreateScheduledTrigger {..} = do
  let headerConfs = fromMaybe [] stHeaders
  webhookInfo <- getWebhookInfoFromConf stWebhookConf
  headerInfo <- getHeaderInfosFromConf headerConfs
  let stInfo =
        ScheduledTriggerInfo
          stName
          stSchedule
          stPayload
          stRetryConf
          webhookInfo
          headerInfo
  pure stInfo

runUpdateScheduledTrigger :: CacheBuildM m => CreateScheduledTrigger ->  m EncJSON
runUpdateScheduledTrigger q = do
  sti <- scheduledTriggerSetup q
  updateScheduledTriggerInCache sti
  updateScheduledTriggerInCatalog q
  return successMsg

updateScheduledTriggerInCatalog :: CacheBuildM m => CreateScheduledTrigger -> m ()
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

runDeleteScheduledTrigger :: CacheBuildM m => DeleteScheduledTrigger -> m EncJSON
runDeleteScheduledTrigger (DeleteScheduledTrigger stName) = do
  removeScheduledTriggerFromCache stName
  deleteScheduledTriggerFromCatalog stName
  return successMsg

deleteScheduledTriggerFromCatalog :: CacheBuildM m => TriggerName -> m ()
deleteScheduledTriggerFromCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_trigger
    WHERE name = $1
   |] (Identity stName) False

runCancelScheduledEvent :: CacheBuildM m => CancelScheduledEvent -> m EncJSON
runCancelScheduledEvent se = do
  affectedRows <- deleteScheduledEventFromCatalog se
  if | affectedRows == 1 -> pure successMsg
     | affectedRows == 0 -> throw400 NotFound "scheduled event not found"
     | otherwise -> throw500 "unexpected: more than one scheduled events cancelled"

deleteScheduledEventFromCatalog :: CacheBuildM m => CancelScheduledEvent -> m Int
deleteScheduledEventFromCatalog se = liftTx $ do
  (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_events
    WHERE id = $1
    RETURNING count(*)
   |] (Identity (cseId se)) False
