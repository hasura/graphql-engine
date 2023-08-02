module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateCronTrigger,
    runDeleteCronTrigger,
    dropCronTriggerInMetadata,
    resolveCronTrigger,
    runCreateScheduledEvent,
    runDeleteScheduledEvent,
    runGetScheduledEvents,
    runGetScheduledEventInvocations,
    populateInitialCronTriggerEvents,
    runGetCronTriggers,
  )
where

import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Time.Clock qualified as C
import Data.URL.Template (printTemplate)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.ScheduledTrigger
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.EventTrigger (getHeaderInfosFromConf)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import System.Cron.Types (CronSchedule)

populateInitialCronTriggerEvents ::
  ( MonadIO m,
    MonadError QErr m,
    MonadMetadataStorage m
  ) =>
  CronSchedule ->
  TriggerName ->
  m ()
populateInitialCronTriggerEvents schedule triggerName = do
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 schedule
  liftEitherM $ insertCronEvents $ map (CronEventSeed triggerName) scheduleTimes

-- | runCreateCronTrigger will update a existing cron trigger when the 'replace'
--   value is set to @true@ and when replace is @false@ a new cron trigger will
--   be created
runCreateCronTrigger ::
  ( MonadError QErr m,
    CacheRWM m,
    MonadIO m,
    MetadataM m,
    MonadMetadataStorage m
  ) =>
  CreateCronTrigger ->
  m EncJSON
runCreateCronTrigger CreateCronTrigger {..} = do
  let q =
        CronTriggerMetadata
          _cctName
          _cctWebhook
          _cctCronSchedule
          _cctPayload
          _cctRetryConf
          _cctHeaders
          _cctIncludeInMetadata
          _cctComment
          _cctRequestTransform
          _cctResponseTransform
  case _cctReplace of
    True -> updateCronTrigger q
    False -> do
      cronTriggersMap <- scCronTriggers <$> askSchemaCache
      case HashMap.lookup (ctName q) cronTriggersMap of
        Nothing -> pure ()
        Just _ ->
          throw400 AlreadyExists
            $ "cron trigger with name: "
            <> triggerNameToTxt (ctName q)
            <> " already exists"

      let metadataObj = MOCronTrigger _cctName
          metadata =
            CronTriggerMetadata
              _cctName
              _cctWebhook
              _cctCronSchedule
              _cctPayload
              _cctRetryConf
              _cctHeaders
              _cctIncludeInMetadata
              _cctComment
              _cctRequestTransform
              _cctResponseTransform
      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ metaCronTriggers
        %~ InsOrdHashMap.insert _cctName metadata
      populateInitialCronTriggerEvents _cctCronSchedule _cctName
      return successMsg

resolveCronTrigger ::
  (QErrM m) =>
  Env.Environment ->
  CronTriggerMetadata ->
  m CronTriggerInfo
resolveCronTrigger env CronTriggerMetadata {..} = do
  webhookInfo <- resolveWebhook env ctWebhook
  headerInfo <- getHeaderInfosFromConf env ctHeaders
  let urlTemplate = printTemplate $ unInputWebhook ctWebhook
  pure
    $ CronTriggerInfo
      ctName
      ctSchedule
      ctPayload
      ctRetryConf
      (EnvRecord urlTemplate webhookInfo)
      headerInfo
      ctComment
      ctRequestTransform
      ctResponseTransform

updateCronTrigger ::
  ( MonadError QErr m,
    CacheRWM m,
    MonadIO m,
    MetadataM m,
    MonadMetadataStorage m
  ) =>
  CronTriggerMetadata ->
  m EncJSON
updateCronTrigger cronTriggerMetadata = do
  let triggerName = ctName cronTriggerMetadata
  checkExists triggerName
  buildSchemaCacheFor (MOCronTrigger triggerName)
    $ MetadataModifier
    $ metaCronTriggers
    %~ InsOrdHashMap.insert triggerName cronTriggerMetadata
  liftEitherM $ dropFutureCronEvents $ SingleCronTrigger triggerName
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 $ ctSchedule cronTriggerMetadata
  liftEitherM $ insertCronEvents $ map (CronEventSeed triggerName) scheduleTimes
  pure successMsg

runDeleteCronTrigger ::
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m,
    MonadMetadataStorage m
  ) =>
  ScheduledTriggerName ->
  m EncJSON
runDeleteCronTrigger (ScheduledTriggerName stName) = do
  checkExists stName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropCronTriggerInMetadata stName
  liftEitherM $ dropFutureCronEvents $ SingleCronTrigger stName
  return successMsg

dropCronTriggerInMetadata :: TriggerName -> MetadataModifier
dropCronTriggerInMetadata name =
  MetadataModifier $ metaCronTriggers %~ InsOrdHashMap.delete name

runCreateScheduledEvent ::
  (MonadError QErr m, MonadMetadataStorage m) =>
  CreateScheduledEvent ->
  m EncJSON
runCreateScheduledEvent scheduledEvent = do
  eid <- liftEitherM $ createOneOffScheduledEvent scheduledEvent
  pure $ encJFromJValue $ J.object ["message" J..= J.String "success", "event_id" J..= eid]

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  cronTriggersMap <- scCronTriggers <$> askSchemaCache
  void
    $ onNothing (HashMap.lookup name cronTriggersMap)
    $ throw400 NotExists
    $ "cron trigger with name: "
    <> triggerNameToTxt name
    <> " does not exist"

runDeleteScheduledEvent ::
  (MonadMetadataStorage m, MonadError QErr m) => DeleteScheduledEvent -> m EncJSON
runDeleteScheduledEvent DeleteScheduledEvent {..} = do
  liftEitherM $ dropEvent _dseEventId _dseType
  pure successMsg

runGetScheduledEvents ::
  ( MonadError QErr m,
    CacheRM m,
    MonadMetadataStorage m
  ) =>
  GetScheduledEvents ->
  m EncJSON
runGetScheduledEvents gse = do
  case _gseScheduledEvent gse of
    SEOneOff -> pure ()
    SECron name -> checkExists name
  encJFromJValue <$> liftEitherM (fetchScheduledEvents gse)

runGetScheduledEventInvocations ::
  ( MonadError QErr m,
    CacheRM m,
    MonadMetadataStorage m
  ) =>
  GetScheduledEventInvocations ->
  m EncJSON
runGetScheduledEventInvocations getEventInvocations@GetScheduledEventInvocations {..} = do
  case _geiInvocationsBy of
    GIBEventId _ _ -> pure ()
    GIBEvent event -> case event of
      SEOneOff -> pure ()
      SECron name -> checkExists name
  WithOptionalTotalCount countMaybe invocations <- liftEitherM $ fetchScheduledEventInvocations getEventInvocations
  pure
    $ encJFromJValue
    $ J.object
    $ ("invocations" J..= invocations)
    : (maybe mempty (\count -> ["count" J..= count]) countMaybe)

-- | Metadata API handler to retrieve all the cron triggers from the metadata
runGetCronTriggers :: (MetadataM m) => m EncJSON
runGetCronTriggers = do
  cronTriggers <- toList . _metaCronTriggers <$> getMetadata
  pure
    $ encJFromJValue
    $ J.object
      ["cron_triggers" J..= cronTriggers]
