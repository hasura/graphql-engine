module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateCronTrigger
  , runUpdateCronTrigger
  , runDeleteCronTrigger
  , addCronTriggerToCatalog
  , deleteCronTriggerFromCatalog
  , resolveCronTrigger
  , runCreateScheduledTriggerOneOff
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger ( getWebhookInfoFromConf
                                             , getHeaderInfosFromConf)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog    (CatalogCronTrigger(..))
import           Hasura.Eventing.ScheduledTrigger

import qualified Database.PG.Query     as Q
import qualified Data.Time.Clock       as C
import qualified Data.HashMap.Strict   as Map

runCreateCronTrigger :: (CacheRWM m, MonadTx m) => CreateCronTrigger ->  m EncJSON
runCreateCronTrigger CreateCronTrigger {..} = do
  let q = (CronTriggerMetadata cctName
                               cctWebhook
                               cctCronSchedule
                               cctPayload
                               cctRetryConf
                               cctHeaders
                               cctIncludeInMetadata
                               cctComment)
  cronTriggersMap <- scCronTriggers <$> askSchemaCache
  case Map.lookup (stName q) cronTriggersMap of
    Nothing -> pure ()
    Just _ -> throw400 AlreadyExists $
      "cron trigger with name: " <> (triggerNameToTxt $ stName q) <> " already exists"

  addCronTriggerToCatalog q
  buildSchemaCacheFor $ MOCronTrigger $ stName q
  return successMsg

addCronTriggerToCatalog :: (MonadTx m) => CronTriggerMetadata ->  m ()
addCronTriggerToCatalog CronTriggerMetadata {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_cron_triggers
        (name, webhook_conf, cron_schedule, payload, retry_conf, header_conf, include_in_metadata, comment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    |] (stName, Q.AltJ stWebhook, stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
       ,Q.AltJ stHeaders, stIncludeInMetadata, stComment) False
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 stSchedule -- generate next 100 events
      events = map (ScheduledEventSeed stName) scheduleTimes
  insertScheduledEvents events

resolveCronTrigger
  :: (QErrM m, MonadIO m)
  => CatalogCronTrigger -> m CronTriggerInfo
resolveCronTrigger CatalogCronTrigger {..} = do
  webhookInfo <- getWebhookInfoFromConf _cctWebhookConf
  headerInfo <- getHeaderInfosFromConf headers
  pure $
    CronTriggerInfo _cctName
                    _cctCronSchedule
                    _cctPayload
                    retryConf
                    webhookInfo
                    headerInfo
                    _cctComment
  where
    retryConf = fromMaybe defaultSTRetryConf _cctRetryConf

    headers = fromMaybe [] _cctHeaderConf

runUpdateCronTrigger :: (CacheRWM m, MonadTx m) => CreateCronTrigger -> m EncJSON
runUpdateCronTrigger CreateCronTrigger {..} = do
  let q = (CronTriggerMetadata cctName
                                    cctWebhook
                                    cctCronSchedule
                                    cctPayload
                                    cctRetryConf
                                    cctHeaders
                                    cctIncludeInMetadata
                                    cctComment)
  checkExists cctName
  updateScheduledTriggerInCatalog q
  buildSchemaCacheFor $ MOCronTrigger $ stName q
  return successMsg

updateScheduledTriggerInCatalog :: (MonadTx m) => CronTriggerMetadata -> m ()
updateScheduledTriggerInCatalog CronTriggerMetadata {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_cron_triggers
    SET webhook_conf = $2,
        cron_schedule = $3,
        payload = $4,
        retry_conf = $5,
        include_in_metadata = $6,
        comment = $7
    WHERE name = $1
   |] (stName, Q.AltJ stWebhook, stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
      , stIncludeInMetadata, stComment) False
  -- since the cron trigger is updated, clear all its future events which are not retries
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_events
    WHERE name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity stName) False

runDeleteCronTrigger :: (CacheRWM m, MonadTx m) => ScheduledTriggerName -> m EncJSON
runDeleteCronTrigger (ScheduledTriggerName stName) = do
  checkExists stName
  deleteCronTriggerFromCatalog stName
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

deleteCronTriggerFromCatalog :: (MonadTx m) => TriggerName -> m ()
deleteCronTriggerFromCatalog stName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_trigger
    WHERE name = $1
   |] (Identity stName) False

runCreateScheduledTriggerOneOff :: (MonadTx m) => CreateScheduledTriggerOneOff -> m EncJSON
runCreateScheduledTriggerOneOff CreateScheduledTriggerOneOff {..} = do
  liftTx $ Q.unitQE defaultTxErrorHandler
     [Q.sql|
      INSERT INTO hdb_catalog.hdb_scheduled_events
      (webhook_conf,scheduled_time,payload,retry_conf,header_conf,comment)
      VALUES
      ($1, $2, $3, $4, $5, $6)
     |] ( Q.AltJ cstoWebhook
        , cstoScheduleAt
        , Q.AltJ cstoPayload
        , Q.AltJ cstoRetryConf
        , Q.AltJ cstoHeaders
        , cstoComment)
        False
  pure successMsg

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  cronTriggersMap <- scCronTriggers <$> askSchemaCache
  void $ onNothing (Map.lookup name cronTriggersMap) $
    throw400 NotExists $
      "cron trigger with name: " <> (triggerNameToTxt name) <> " does not exist"
