module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateCronTrigger
  , runDeleteCronTrigger
  , dropCronTriggerInMetadata
  , resolveCronTrigger
  , runCreateScheduledEvent
  , runDeleteScheduledEvent
  , runGetScheduledEvents
  , runGetEventInvocations
  ) where

import           Hasura.EncJSON
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger      (getHeaderInfosFromConf)
import           Hasura.RQL.Types

import qualified Data.Aeson                       as J
import qualified Data.Environment                 as Env
import qualified Data.HashMap.Strict              as Map
import qualified Data.HashMap.Strict.InsOrd       as OMap
import qualified Data.Time.Clock                  as C

-- | runCreateCronTrigger will update a existing cron trigger when the 'replace'
--   value is set to @true@ and when replace is @false@ a new cron trigger will
--   be created
runCreateCronTrigger
  :: ( CacheRWM m, MonadIO m
     , MetadataM m, MonadMetadataStorageQueryAPI m
     )
  => CreateCronTrigger ->  m EncJSON
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
        createScheduledEvent $ SESCron $ map (CronEventSeed cctName) scheduleTimes
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
     , MonadIO m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => CronTriggerMetadata -> m EncJSON
updateCronTrigger cronTriggerMetadata = do
  let triggerName = ctName cronTriggerMetadata
  checkExists triggerName
  buildSchemaCacheFor (MOCronTrigger triggerName)
    $ MetadataModifier
    $ metaCronTriggers %~ OMap.insert triggerName cronTriggerMetadata
  dropFutureCronEvents triggerName
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 $ ctSchedule cronTriggerMetadata
  createScheduledEvent $ SESCron $ map (CronEventSeed triggerName) scheduleTimes
  pure successMsg

runDeleteCronTrigger
  :: ( CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => ScheduledTriggerName -> m EncJSON
runDeleteCronTrigger (ScheduledTriggerName stName) = do
  checkExists stName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropCronTriggerInMetadata stName
  dropFutureCronEvents stName
  return successMsg

dropCronTriggerInMetadata :: TriggerName -> MetadataModifier
dropCronTriggerInMetadata name =
  MetadataModifier $ metaCronTriggers %~ OMap.delete name

runCreateScheduledEvent
  :: (MonadMetadataStorageQueryAPI m) => CreateScheduledEvent -> m EncJSON
runCreateScheduledEvent =
  (createScheduledEvent . SESOneOff) >=> \() -> pure successMsg

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  cronTriggersMap <- scCronTriggers <$> askSchemaCache
  void $ onNothing (Map.lookup name cronTriggersMap) $
    throw400 NotExists $
      "cron trigger with name: " <> triggerNameToTxt name <> " does not exist"

runDeleteScheduledEvent
  :: (MonadMetadataStorageQueryAPI m) => DeleteScheduledEvent -> m EncJSON
runDeleteScheduledEvent DeleteScheduledEvent{..} = do
  dropEvent _dseEventId _dseType
  pure successMsg

runGetScheduledEvents
  :: ( CacheRM m
     , MonadMetadataStorageQueryAPI m
     )
  => GetScheduledEvents -> m EncJSON
runGetScheduledEvents gse = do
  case _gseScheduledEvent gse of
    SEOneOff    -> pure ()
    SECron name -> checkExists name
  encJFromJValue <$> fetchScheduledEvents gse

runGetEventInvocations
  :: ( CacheRM m
     , MonadMetadataStorageQueryAPI m
     )
  => GetEventInvocations -> m EncJSON
runGetEventInvocations GetEventInvocations{..} = do
  case _geiInvocationsBy of
    GIBEventId _ _ -> pure ()
    GIBEvent event -> case event of
      SEOneOff    -> pure ()
      SECron name -> checkExists name
  WithTotalCount count invocations <- fetchInvocations _geiInvocationsBy _geiPagination
  pure $ encJFromJValue $ J.object [ "invocations" J..= invocations
                                   , "count" J..= count
                                   ]
