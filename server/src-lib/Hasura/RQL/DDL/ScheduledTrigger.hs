module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , runUpdateScheduledTrigger
  , runDeleteScheduledTrigger
  , runCreateScheduledEvent
  , addScheduledTriggerToCatalog
  , deleteScheduledTriggerFromCatalog
  , resolveScheduledTrigger
  , runFetchEventsOfScheduledTrigger
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

runCreateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTrigger ->  m EncJSON
runCreateScheduledTrigger q = do
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
        (name, webhook_conf, schedule_conf, payload, retry_conf, header_conf, include_in_metadata)
      VALUES ($1, $2, $3, $4, $5, $6, $7)
    |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
       ,Q.AltJ stHeaders, stIncludeInMetadata) False
  case stSchedule of
    AdHoc Nothing -> pure ()
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

runUpdateScheduledTrigger :: (CacheRWM m, MonadTx m) => CreateScheduledTrigger -> m EncJSON
runUpdateScheduledTrigger q = do
  checkExists (stName q)
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
        include_in_metadata = $6
    WHERE name = $1
   |] (stName, Q.AltJ stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf
      , stIncludeInMetadata) False
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

checkExists :: (CacheRM m, MonadError QErr m) => TriggerName -> m ()
checkExists name = do
  stMap <- scScheduledTriggers <$> askSchemaCache
  void $ onNothing (Map.lookup name stMap) $
    throw400 NotExists $
      "scheduled trigger with name: " <> (triggerNameToTxt name) <> " does not exist"
