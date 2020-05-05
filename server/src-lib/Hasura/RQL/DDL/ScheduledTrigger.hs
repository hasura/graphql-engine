module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTriggerCron
  , runUpdateScheduledTriggerCron
  , runDeleteScheduledTrigger
  , runCreateScheduledEvent
  , addScheduledTriggerToCatalog
  , deleteScheduledTriggerFromCatalog
  , resolveScheduledTrigger
  , runFetchEventsOfScheduledTrigger
  , runCreateScheduledTriggerOneOff
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger ( getWebhookInfoFromConf
                                             , getHeaderInfosFromConf)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog    (CatalogScheduledTrigger(..))
import           Hasura.Eventing.ScheduledTrigger

import qualified Database.PG.Query     as Q
import qualified Data.Time.Clock       as C
import qualified Data.HashMap.Strict   as Map
import qualified Data.Aeson            as J
import qualified Data.Aeson.Casing     as J
import qualified Data.Aeson.TH         as J

data FetchEventsResponse
  = FetchEventsResponse
  { feerScheduledEvents :: ![ScheduledEventDb]
  } deriving (Eq, Show)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''FetchEventsResponse)

runCreateScheduledTriggerCron :: (CacheRWM m, MonadTx m) => CreateScheduledTriggerCron ->  m EncJSON
runCreateScheduledTriggerCron CreateScheduledTriggerCron {..} = do
  let q = (CreateScheduledTrigger stcName
                                  stcWebhook
                                  (Cron stcCronSchedule)
                                  stcPayload
                                  stcRetryConf
                                  stcHeaders
                                  stcIncludeInMetadata
                                  stcComment)
  stMap <- scScheduledTriggers <$> askSchemaCache
  case Map.lookup (stName q) stMap of
    Nothing -> pure ()
    Just _ -> throw400 AlreadyExists $
      "scheduled trigger with name: " <> (triggerNameToTxt $ stName q) <> " already exists"

  addScheduledTriggerToCatalog q
  buildSchemaCacheFor $ MOScheduledTrigger $ stName q
  return successMsg

addScheduledTriggerToCatalog :: (MonadTx m) => CreateScheduledTrigger ->  m ()
addScheduledTriggerToCatalog CreateScheduledTrigger {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_scheduled_trigger
        (name, webhook_conf, schedule_conf, payload, retry_conf, header_conf, include_in_metadata, comment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
       ,Q.AltJ stHeaders, stIncludeInMetadata, stComment) False
  case stSchedule of
    OneOff -> pure ()
    Cron cron -> do
      currentTime <- liftIO C.getCurrentTime
      let scheduleTimes = generateScheduleTimes currentTime 100 cron -- generate next 100 events
          events = map (ScheduledEventSeed stName) scheduleTimes
      insertScheduledEvents events

resolveScheduledTrigger
  :: (QErrM m, MonadIO m)
  => CatalogScheduledTrigger -> m ScheduledTriggerInfo
resolveScheduledTrigger CatalogScheduledTrigger {..} = do
  webhookInfo <- getWebhookInfoFromConf _cstWebhookConf
  headerInfo <- getHeaderInfosFromConf headers
  pure $
    ScheduledTriggerInfo _cstName
                         _cstScheduleConf
                         _cstPayload
                         retryConf
                         webhookInfo
                         headerInfo
  where
    retryConf = fromMaybe defaultSTRetryConf _cstRetryConf

    headers = fromMaybe [] _cstHeaderConf

runUpdateScheduledTriggerCron :: (CacheRWM m, MonadTx m) => CreateScheduledTriggerCron -> m EncJSON
runUpdateScheduledTriggerCron CreateScheduledTriggerCron {..} = do
  let q = (CreateScheduledTrigger stcName
                                  stcWebhook
                                  (Cron stcCronSchedule)
                                  stcPayload
                                  stcRetryConf
                                  stcHeaders
                                  stcIncludeInMetadata
                                  stcComment)
  checkExists stcName
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
        retry_conf = $5,
        include_in_metadata = $6,
        comment = $7
    WHERE name = $1
   |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
      , stIncludeInMetadata, stComment) False
  -- since the scheduled trigger is updated, clear all its future events which are not retries
  Q.unitQE defaultTxErrorHandler
   [Q.sql|
    DELETE FROM hdb_catalog.hdb_scheduled_events
    WHERE name = $1 AND scheduled_time > now() AND tries = 0
   |] (Identity stName) False

runDeleteScheduledTrigger :: (CacheRWM m, MonadTx m) => ScheduledTriggerName -> m EncJSON
runDeleteScheduledTrigger (ScheduledTriggerName stName) = do
  checkExists stName
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

runCreateScheduledEvent :: (CacheRM m, MonadTx m) => CreateScheduledEvent -> m EncJSON
runCreateScheduledEvent CreateScheduledEvent{..} = do
  checkExists steName
  liftTx $ Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_scheduled_events
        (name, scheduled_time, additional_payload)
       VALUES ($1, $2, $3)
    |] (steName, steTimestamp, Q.AltJ <$> stePayload) False
  pure successMsg

runFetchEventsOfScheduledTrigger
    :: (CacheRM m, MonadTx m) => FetchEventsScheduledTrigger -> m EncJSON
runFetchEventsOfScheduledTrigger (FetchEventsScheduledTrigger stName stOffset stLimit) = do
  checkExists stName
  events <- liftTx $ map uncurryScheduledEvent <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
      SELECT id,name,scheduled_time,additional_payload,status,tries
      FROM hdb_catalog.hdb_scheduled_events
      WHERE name = $1
      ORDER BY scheduled_time
      OFFSET $2
      LIMIT $3
     |] (stName,stOffset,stLimit) True
  pure $ encJFromJValue $ J.toJSON $ FetchEventsResponse events
  where
    uncurryScheduledEvent (seId,name,scheduledTime,payload,status,tries) =
      ScheduledEventDb seId name scheduledTime (Q.getAltJ <$> payload) status tries

runCreateScheduledTriggerOneOff :: (MonadTx m) => CreateScheduledTriggerOneOff -> m EncJSON
runCreateScheduledTriggerOneOff CreateScheduledTriggerOneOff {..} = do
  liftTx $ Q.unitQE defaultTxErrorHandler
     [Q.sql|
      INSERT INTO hdb_catalog.hdb_one_off_scheduled_events
      (webhook_conf,scheduled_time,retry_conf,payload,comment)
      VALUES
      ($1, $2, $3, $4, $5)
     |] ( Q.AltJ cstoWebhook
        , cstoScheduleAt
        , Q.AltJ cstoPayload
        , Q.AltJ cstoRetryConf
        , cstoComment)
        False
  pure successMsg

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  stMap <- scScheduledTriggers <$> askSchemaCache
  void $ onNothing (Map.lookup name stMap) $
    throw400 NotExists $
      "scheduled trigger with name: " <> (triggerNameToTxt name) <> " does not exist"
