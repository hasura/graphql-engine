module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateCronTrigger
  , runDeleteCronTrigger
  , addCronTriggerToCatalog
  , deleteCronTriggerFromCatalog
  , resolveCronTrigger
  , runCreateScheduledEvent
  ) where

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger         (getHeaderInfosFromConf)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog            (CatalogCronTrigger (..))

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as Map
import qualified Data.Time.Clock                     as C
import qualified Database.PG.Query                   as Q

-- | runCreateCronTrigger will update a existing cron trigger when the 'replace'
--   value is set to @true@ and when replace is @false@ a new cron trigger will
--   be created
runCreateCronTrigger :: (CacheRWM m, MonadTx m) => CreateCronTrigger ->  m EncJSON
runCreateCronTrigger CreateCronTrigger {..} = do
  let q = CronTriggerMetadata cctName
                               cctWebhook
                               cctCronSchedule
                               cctPayload
                               cctRetryConf
                               cctHeaders
                               cctIncludeInMetadata
                               cctComment
  case cctReplace of
    True -> updateCronTrigger q
    False -> do
        cronTriggersMap <- scCronTriggers <$> askSchemaCache
        case Map.lookup (ctName q) cronTriggersMap of
          Nothing -> pure ()
          Just _ -> throw400 AlreadyExists $
                    "cron trigger with name: "
                    <> triggerNameToTxt (ctName q)
                    <> " already exists"

        addCronTriggerToCatalog q
        buildSchemaCacheFor $ MOCronTrigger $ ctName q
        return successMsg

addCronTriggerToCatalog :: (MonadTx m) => CronTriggerMetadata ->  m ()
addCronTriggerToCatalog CronTriggerMetadata {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_cron_triggers
        (name, webhook_conf, cron_schedule, payload, retry_conf, header_conf, include_in_metadata, comment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    |] (ctName, Q.AltJ ctWebhook, ctSchedule, Q.AltJ <$> ctPayload, Q.AltJ ctRetryConf
       ,Q.AltJ ctHeaders, ctIncludeInMetadata, ctComment) False
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 ctSchedule -- generate next 100 events
  insertScheduledEventTx $ SESCron $ map (CronEventSeed ctName) scheduleTimes

resolveCronTrigger
  :: (QErrM m)
  => Env.Environment
  -> CatalogCronTrigger
  -> m CronTriggerInfo
resolveCronTrigger env CatalogCronTrigger {..} = do
  webhookInfo <- resolveWebhook env _cctWebhookConf
  headerInfo <- getHeaderInfosFromConf env headers
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

updateCronTrigger :: (CacheRWM m, MonadTx m) => CronTriggerMetadata -> m EncJSON
updateCronTrigger cronTriggerMetadata = do
  checkExists $ ctName cronTriggerMetadata
  updateCronTriggerInCatalog cronTriggerMetadata
  buildSchemaCacheFor $ MOCronTrigger $ ctName cronTriggerMetadata
  return successMsg

updateCronTriggerInCatalog :: (MonadTx m) => CronTriggerMetadata -> m ()
updateCronTriggerInCatalog CronTriggerMetadata {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    UPDATE hdb_catalog.hdb_cron_triggers
    SET webhook_conf = $2,
        cron_schedule = $3,
        payload = $4,
        retry_conf = $5,
        header_conf = $6,
        include_in_metadata = $7,
        comment = $8
    WHERE name = $1
   |] (ctName, Q.AltJ ctWebhook, ctSchedule, Q.AltJ <$> ctPayload, Q.AltJ ctRetryConf,Q.AltJ ctHeaders
      , ctIncludeInMetadata, ctComment) False
  -- since the cron trigger is updated, clear all its future events which are not retries
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_events
    WHERE trigger_name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity ctName) False
  -- create the next 100 cron events, as the future events were deleted
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 ctSchedule
  insertScheduledEventTx $ SESCron $ map (CronEventSeed ctName) scheduleTimes

runDeleteCronTrigger :: (CacheRWM m, MonadTx m) => ScheduledTriggerName -> m EncJSON
runDeleteCronTrigger (ScheduledTriggerName stName) = do
  checkExists stName
  deleteCronTriggerFromCatalog stName
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

deleteCronTriggerFromCatalog :: (MonadTx m) => TriggerName -> m ()
deleteCronTriggerFromCatalog triggerName = liftTx $ do
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_triggers
    WHERE name = $1
   |] (Identity triggerName) False

runCreateScheduledEvent :: (MonadTx m) => CreateScheduledEvent -> m EncJSON
runCreateScheduledEvent CreateScheduledEvent {..} = do
  liftTx $ Q.unitQE defaultTxErrorHandler
     [Q.sql|
      INSERT INTO hdb_catalog.hdb_scheduled_events
      (webhook_conf,scheduled_time,payload,retry_conf,header_conf,comment)
      VALUES
      ($1, $2, $3, $4, $5, $6)
     |] ( Q.AltJ cseWebhook
        , cseScheduleAt
        , Q.AltJ csePayload
        , Q.AltJ cseRetryConf
        , Q.AltJ cseHeaders
        , cseComment)
        False
  pure successMsg

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  cronTriggersMap <- scCronTriggers <$> askSchemaCache
  void $ onNothing (Map.lookup name cronTriggersMap) $
    throw400 NotExists $
      "cron trigger with name: " <> triggerNameToTxt name <> " does not exist"
