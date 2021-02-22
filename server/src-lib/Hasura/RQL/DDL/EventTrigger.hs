module Hasura.RQL.DDL.EventTrigger
  ( CreateEventTriggerQuery
  , runCreateEventTriggerQuery
  , DeleteEventTriggerQuery
  , runDeleteEventTriggerQuery
  , dropEventTriggerInMetadata
  , RedeliverEventQuery
  , runRedeliverEvent
  , runInvokeEventTrigger

  -- TODO(from master): review
  , archiveEvents
  , getEventTriggerDef
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as HM
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Database.PG.Query                      as Q

import           Control.Lens                           ((.~))
import           Data.Aeson
import           Data.Text.Extended


import           Hasura.Backends.Postgres.DDL.Table
import           Hasura.Backends.Postgres.Execute.Types
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Session

archiveEvents :: TriggerName -> Q.TxE QErr ()
archiveEvents trn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |] (Identity trn) False

checkEvent :: EventId -> Q.TxE QErr ()
checkEvent eid = do
  events <- Q.listQE defaultTxErrorHandler
            [Q.sql|
              SELECT l.locked IS NOT NULL AND l.locked >= (NOW() - interval '30 minute')
              FROM hdb_catalog.event_log l
              WHERE l.id = $1
              |] (Identity eid) True
  event <- getEvent events
  assertEventUnlocked event
  where
    getEvent []    = throw400 NotExists "event not found"
    getEvent (x:_) = return x

    assertEventUnlocked (Identity locked) = when locked $
      throw400 Busy "event is already being processed"

markForDelivery :: EventId -> Q.TxE QErr ()
markForDelivery eid =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET
          delivered = 'f',
          error = 'f',
          tries = 0
          WHERE id = $1
          |] (Identity eid) True

resolveEventTriggerQuery :: (UserInfoM m, QErrM m, CacheRM m) => CreateEventTriggerQuery -> m (TableCoreInfo 'Postgres, Bool, EventTriggerConf)
resolveEventTriggerQuery (CreateEventTriggerQuery source name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace) = do
  ti <- askTableCoreInfo source qt
  -- can only replace for same table
  when replace $ do
    ti' <- _tiCoreInfo <$> askTabInfoFromTrigger source name
    when (_tciName ti' /= _tciName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (ti, replace, EventTriggerConf name (TriggerOpsDef insert update delete enableManual) webhook webhookFromEnv rconf mheaders)
  where
    assertCols ti opSpec = onJust opSpec \sos -> case sosColumns sos of
      SubCStar         -> return ()
      SubCArray pgcols -> forM_ pgcols (assertPGCol (_tciFieldInfoMap ti) "")

createEventTriggerQueryMetadata
  :: (QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => CreateEventTriggerQuery -> m (TableCoreInfo 'Postgres, EventTriggerConf)
createEventTriggerQueryMetadata q = do
  (tableCoreInfo, replace, triggerConf) <- resolveEventTriggerQuery q
  let table = cetqTable q
      source = cetqSource q
      triggerName = etcName triggerConf
      metadataObj = MOSourceObjId source $ SMOTableObj table $ MTOTrigger triggerName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source table.tmEventTriggers %~
      if replace then ix triggerName .~ triggerConf
      else OMap.insert triggerName triggerConf
  pure (tableCoreInfo, triggerConf)

runCreateEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => CreateEventTriggerQuery -> m EncJSON
runCreateEventTriggerQuery q = do
  void $ createEventTriggerQueryMetadata q
  pure successMsg

runDeleteEventTriggerQuery
  :: (MonadError QErr m, CacheRWM m, MonadIO m, MetadataM m)
  => DeleteEventTriggerQuery -> m EncJSON
runDeleteEventTriggerQuery (DeleteEventTriggerQuery source name) = do
  -- liftTx $ delEventTriggerFromCatalog name
  sourceInfo <- askSourceInfo source
  let maybeTable = HM.lookup name $ HM.unions $
        flip map (HM.toList $ _siTables sourceInfo) $ \(table, tableInfo) ->
        HM.map (const table) $ _tiEventTriggerInfoMap tableInfo
  table <- onNothing maybeTable $ throw400 NotExists $
           "event trigger with name " <> name <<> " not exists"

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ tableMetadataSetter source table %~ dropEventTriggerInMetadata name

  liftEitherM $ liftIO $ runPgSourceWriteTx (_siConfiguration sourceInfo) $ do
    delTriggerQ name
    archiveEvents name
  pure successMsg

dropEventTriggerInMetadata :: TriggerName -> TableMetadata b -> TableMetadata b
dropEventTriggerInMetadata name =
  tmEventTriggers %~ OMap.delete name

deliverEvent ::EventId -> Q.TxE QErr ()
deliverEvent eventId = do
  checkEvent eventId
  markForDelivery eventId

runRedeliverEvent
  :: (MonadIO m, CacheRM m, QErrM m)
  => RedeliverEventQuery -> m EncJSON
runRedeliverEvent (RedeliverEventQuery eventId source) = do
  sourceConfig <- askSourceConfig source
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $ deliverEvent eventId
  pure successMsg

insertManualEvent
  :: QualifiedTable
  -> TriggerName
  -> Value
  -> Q.TxE QErr EventId
insertManualEvent qt trn rowData = do
  let op = tshow MANUAL
  eids <- map runIdentity <$> Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
                |] (sn, tn, trn, op, Q.AltJ $ toJSON rowData) True
  getEid eids
  where
    QualifiedObject sn tn = qt
    getEid []    = throw500 "could not create manual event"
    getEid (x:_) = return x

runInvokeEventTrigger
  :: (MonadIO m, QErrM m, CacheRM m)
  => InvokeEventTriggerQuery -> m EncJSON
runInvokeEventTrigger (InvokeEventTriggerQuery name source payload) = do
  trigInfo <- askEventTriggerInfo source name
  assertManual $ etiOpsDef trigInfo
  ti  <- askTabInfoFromTrigger source name
  sourceConfig <- askSourceConfig source
  eid <- liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $
         insertManualEvent (_tciName $ _tiCoreInfo ti) name payload
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _         -> throw400 NotSupported "manual mode is not enabled for event trigger"

getEventTriggerDef
  :: TriggerName
  -> Q.TxE QErr (QualifiedTable, EventTriggerConf)
getEventTriggerDef triggerName = do
  (sn, tn, Q.AltJ etc) <- Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT e.schema_name, e.table_name, e.configuration::json
     FROM hdb_catalog.event_triggers e where e.name = $1
           |] (Identity triggerName) False
  return (QualifiedObject sn tn, etc)

askTabInfoFromTrigger
  :: (QErrM m, CacheRM m)
  => SourceName -> TriggerName -> m (TableInfo 'Postgres)
askTabInfoFromTrigger sourceName trn = do
  sc <- askSchemaCache
  let tabInfos = HM.elems $ fromMaybe mempty $ unsafeTableCache sourceName $ scPostgres sc
  find (isJust . HM.lookup trn . _tiEventTriggerInfoMap) tabInfos
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"

askEventTriggerInfo
  :: (QErrM m, CacheRM m)
  => SourceName -> TriggerName -> m (EventTriggerInfo 'Postgres)
askEventTriggerInfo sourceName trn = do
  ti <- askTabInfoFromTrigger sourceName trn
  let etim = _tiEventTriggerInfoMap ti
  HM.lookup trn etim `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"
