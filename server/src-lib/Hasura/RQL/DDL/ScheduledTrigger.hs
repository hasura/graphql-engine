module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateCronTrigger
  , runDeleteCronTrigger
  , dropCronTriggerInMetadata
  , resolveCronTrigger
  , runCreateScheduledEvent
  ) where

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger         (getHeaderInfosFromConf)
import           Hasura.RQL.Types

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.Time.Clock                     as C
import qualified Database.PG.Query                   as Q

-- | runCreateCronTrigger will update a existing cron trigger when the 'replace'
--   value is set to @true@ and when replace is @false@ a new cron trigger will
--   be created
runCreateCronTrigger
  :: (CacheRWM m, MonadTx m, MonadIO m, MetadataM m) => CreateCronTrigger ->  m EncJSON
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

        let metadataObj = MOCronTrigger cctName
            metadata = CronTriggerMetadata cctName cctWebhook cctCronSchedule
                       cctPayload cctRetryConf cctHeaders cctIncludeInMetadata
                       cctComment
        buildSchemaCacheFor metadataObj
          $ MetadataModifier
          $ metaCronTriggers %~ OMap.insert cctName metadata
        currentTime <- liftIO C.getCurrentTime
        let scheduleTimes = generateScheduleTimes currentTime 100 cctCronSchedule -- generate next 100 events
        liftTx $ insertScheduledEventTx $ SESCron $ map (CronEventSeed cctName) scheduleTimes
        return successMsg

resolveCronTrigger
  :: (QErrM m)
  => Env.Environment
  -> CronTriggerMetadata
  -> m CronTriggerInfo
resolveCronTrigger env CronTriggerMetadata{..} = do
  webhookInfo <- resolveWebhook env ctWebhook
  headerInfo <- getHeaderInfosFromConf env ctHeaders
  pure $
    CronTriggerInfo ctName
                    ctSchedule
                    ctPayload
                    ctRetryConf
                    webhookInfo
                    headerInfo
                    ctComment

updateCronTrigger
  :: ( CacheRWM m
     , MonadTx m
     , MonadIO m
     , MetadataM m
     )
  => CronTriggerMetadata -> m EncJSON
updateCronTrigger cronTriggerMetadata = do
  let triggerName = ctName cronTriggerMetadata
  checkExists triggerName
  buildSchemaCacheFor (MOCronTrigger triggerName)
    $ MetadataModifier
    $ metaCronTriggers %~ OMap.insert triggerName cronTriggerMetadata
  liftTx $ dropFutureCronEvents triggerName
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 $ ctSchedule cronTriggerMetadata
  liftTx $ insertScheduledEventTx $ SESCron $ map (CronEventSeed triggerName) scheduleTimes
  pure successMsg

runDeleteCronTrigger
  :: ( CacheRWM m
     , MonadTx m
     , MetadataM m
     )
  => ScheduledTriggerName -> m EncJSON
runDeleteCronTrigger (ScheduledTriggerName stName) = do
  checkExists stName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropCronTriggerInMetadata stName
  liftTx $ dropFutureCronEvents stName
  return successMsg

dropFutureCronEvents :: TriggerName -> Q.TxE QErr ()
dropFutureCronEvents name =
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_events
    WHERE trigger_name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity name) False

dropCronTriggerInMetadata :: TriggerName -> MetadataModifier
dropCronTriggerInMetadata name =
  MetadataModifier $ metaCronTriggers %~ OMap.delete name

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
