module Hasura.RQL.DDL.EventTrigger
  ( CreateEventTriggerQuery
  , runCreateEventTriggerQuery
  , DeleteEventTriggerQuery
  , runDeleteEventTriggerQuery
  , dropEventTriggerInMetadata
  , RedeliverEventQuery
  , runRedeliverEvent
  , InvokeEventTriggerQuery
  , runInvokeEventTrigger
  -- TODO(from master): review
  , archiveEvents
  , getEventTriggerDef
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Text.Regex.TDFA                    as TDFA

import           Control.Lens                       ((.~))
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.DDL.Table as PG
import qualified Hasura.Backends.Postgres.SQL.Types as PG
import qualified Hasura.SQL.AnyBackend              as AB
import qualified Hasura.Tracing                     as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Session


data CreateEventTriggerQuery (b :: BackendType)
  = CreateEventTriggerQuery
  { _cetqSource         :: !SourceName
  , _cetqName           :: !TriggerName
  , _cetqTable          :: !(TableName b)
  , _cetqInsert         :: !(Maybe SubscribeOpSpec)
  , _cetqUpdate         :: !(Maybe SubscribeOpSpec)
  , _cetqDelete         :: !(Maybe SubscribeOpSpec)
  , _cetqEnableManual   :: !(Maybe Bool)
  , _cetqRetryConf      :: !(Maybe RetryConf)
  , _cetqWebhook        :: !(Maybe InputWebhook)
  , _cetqWebhookFromEnv :: !(Maybe Text)
  , _cetqHeaders        :: !(Maybe [HeaderConf])
  , _cetqReplace        :: !Bool
  }

instance Backend b => FromJSON (CreateEventTriggerQuery b) where
  parseJSON  = withObject "create event trigger" \o -> do
    sourceName      <- o .:? "source" .!= defaultSource
    name            <- o .:  "name"
    table           <- o .:  "table"
    insert          <- o .:? "insert"
    update          <- o .:? "update"
    delete          <- o .:? "delete"
    enableManual    <- o .:? "enable_manual" .!= False
    retryConf       <- o .:? "retry_conf"
    webhook         <- o .:? "webhook"
    webhookFromEnv  <- o .:? "webhook_from_env"
    headers         <- o .:? "headers"
    replace         <- o .:? "replace" .!= False
    let regex = "^[A-Za-z]+[A-Za-z0-9_\\-]*$" :: LBS.ByteString
        compiledRegex = TDFA.makeRegex regex :: TDFA.Regex
        isMatch = TDFA.match compiledRegex . T.unpack $ triggerNameToTxt name
    unless isMatch $
      fail "only alphanumeric and underscore and hyphens allowed for name"
    unless (T.length (triggerNameToTxt name) <= maxTriggerNameLength) $
      fail "event trigger name can be at most 42 characters"
    unless (any isJust [insert, update, delete] || enableManual) $
      fail "atleast one amongst insert/update/delete/enable_manual spec must be provided"
    case (webhook, webhookFromEnv) of
      (Just _, Nothing) -> return ()
      (Nothing, Just _) -> return ()
      (Just _, Just _)  -> fail "only one of webhook or webhook_from_env should be given"
      _                 -> fail "must provide webhook or webhook_from_env"
    mapM_ checkEmptyCols [insert, update, delete]
    return $ CreateEventTriggerQuery sourceName name table insert update delete (Just enableManual) retryConf webhook webhookFromEnv headers replace
    where
      checkEmptyCols spec
        = case spec of
        Just (SubscribeOpSpec (SubCArray cols) _) -> when (null cols) (fail "found empty column specification")
        Just (SubscribeOpSpec _ (Just (SubCArray cols)) ) -> when (null cols) (fail "found empty payload specification")
        _ -> return ()


data DeleteEventTriggerQuery (b :: BackendType)
  = DeleteEventTriggerQuery
  { _detqSource :: !SourceName
  , _detqName   :: !TriggerName
  }

instance FromJSON (DeleteEventTriggerQuery b) where
  parseJSON = withObject "delete event trigger" $ \o ->
    DeleteEventTriggerQuery
      <$> o .:? "source" .!= defaultSource
      <*> o .: "name"


data RedeliverEventQuery (b :: BackendType)
  = RedeliverEventQuery
  { _rdeqEventId :: !EventId
  , _rdeqSource  :: !SourceName
  }

instance FromJSON (RedeliverEventQuery b) where
  parseJSON = withObject "redeliver event trigger" $ \o ->
    RedeliverEventQuery
      <$> o .: "event_id"
      <*> o .:? "source" .!= defaultSource


data InvokeEventTriggerQuery (b :: BackendType)
  = InvokeEventTriggerQuery
  { _ietqName    :: !TriggerName
  , _ietqSource  :: !SourceName
  , _ietqPayload :: !Value
  }

instance Backend b => FromJSON (InvokeEventTriggerQuery b) where
  parseJSON = withObject "invoke event trigger" $ \o ->
    InvokeEventTriggerQuery
      <$> o .: "name"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "payload"


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

resolveEventTriggerQuery
  :: forall pgKind m
   . (Backend ('Postgres pgKind), UserInfoM m, QErrM m, CacheRM m)
  => CreateEventTriggerQuery ('Postgres pgKind)
  -> m (TableCoreInfo ('Postgres pgKind), Bool, EventTriggerConf)
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
  :: forall pgKind m
   . (BackendMetadata ('Postgres pgKind), QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => CreateEventTriggerQuery ('Postgres pgKind)
  -> m (TableCoreInfo ('Postgres pgKind), EventTriggerConf)
createEventTriggerQueryMetadata q = do
  (tableCoreInfo, replace, triggerConf) <- resolveEventTriggerQuery q
  let table = _cetqTable q
      source = _cetqSource q
      triggerName = etcName triggerConf
      metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOTableObj @('Postgres pgKind) table
          $ MTOTrigger triggerName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @('Postgres pgKind) source table.tmEventTriggers %~
      if replace then ix triggerName .~ triggerConf
      else OMap.insert triggerName triggerConf
  pure (tableCoreInfo, triggerConf)

runCreateEventTriggerQuery
  :: forall pgKind m
   . (BackendMetadata ('Postgres pgKind), QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => CreateEventTriggerQuery ('Postgres pgKind)
  -> m EncJSON
runCreateEventTriggerQuery q = do
  void $ createEventTriggerQueryMetadata @pgKind q
  pure successMsg

runDeleteEventTriggerQuery
  :: forall pgKind m
   . (BackendMetadata ('Postgres pgKind), MonadError QErr m, CacheRWM m, MonadIO m, MetadataM m)
  => DeleteEventTriggerQuery ('Postgres pgKind)
  -> m EncJSON
runDeleteEventTriggerQuery (DeleteEventTriggerQuery source name) = do
  -- liftTx $ delEventTriggerFromCatalog name
  sourceInfo <- askSourceInfo source
  let maybeTable = HM.lookup name $ HM.unions $
        flip map (HM.toList $ _siTables @('Postgres pgKind) sourceInfo) $ \(table, tableInfo) ->
        HM.map (const table) $ _tiEventTriggerInfoMap tableInfo
  table <- onNothing maybeTable $ throw400 NotExists $
           "event trigger with name " <> name <<> " does not exist"

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ tableMetadataSetter @('Postgres pgKind) source table %~ dropEventTriggerInMetadata name

  liftEitherM $ liftIO $ runPgSourceWriteTx (_siConfiguration sourceInfo) $ do
    PG.delTriggerQ name
    archiveEvents name
  pure successMsg

dropEventTriggerInMetadata :: TriggerName -> TableMetadata b -> TableMetadata b
dropEventTriggerInMetadata name =
  tmEventTriggers %~ OMap.delete name

deliverEvent :: EventId -> Q.TxE QErr ()
deliverEvent eventId = do
  checkEvent eventId
  markForDelivery eventId

runRedeliverEvent
  :: forall pgKind m
   . (BackendMetadata ('Postgres pgKind), MonadIO m, CacheRM m, QErrM m, MetadataM m)
  => RedeliverEventQuery ('Postgres pgKind)
  -> m EncJSON
runRedeliverEvent (RedeliverEventQuery eventId source) = do
  sourceConfig <- askSourceConfig @('Postgres pgKind) source
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $ deliverEvent eventId
  pure successMsg

insertManualEvent
  :: PG.QualifiedTable
  -> TriggerName
  -> Value
  -> Q.TxE QErr EventId
insertManualEvent (PG.QualifiedObject sn tn) trn rowData = do
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
  |] (sn, tn, trn, (tshow MANUAL), Q.AltJ rowData) False

runInvokeEventTrigger
  :: forall pgKind m
   . ( BackendMetadata ('Postgres pgKind)
     , MonadIO m
     , QErrM m
     , CacheRM m
     , MetadataM m
     , Tracing.MonadTrace m
     , UserInfoM m
     )
  => InvokeEventTriggerQuery ('Postgres pgKind)
  -> m EncJSON
runInvokeEventTrigger (InvokeEventTriggerQuery name source payload) = do
  trigInfo <- askEventTriggerInfo source name
  assertManual $ etiOpsDef trigInfo
  ti  <- askTabInfoFromTrigger source name
  sourceConfig <- askSourceConfig @('Postgres pgKind) source
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  -- NOTE: The methods `setTraceContextInTx` and `setHeadersTx` are being used
  -- to ensure that the trace context and user info are set with valid values
  -- while being used in the PG function `insert_event_log`.
  -- See Issue(#7087) for more details on a bug that was being caused
  -- in the absence of these methods.
  eid <- liftEitherM
          $  liftIO
          $  runPgSourceWriteTx sourceConfig
          $  setHeadersTx (_uiSession userInfo)
          >> setTraceContextInTx traceCtx
          >> insertManualEvent (tableInfoName ti) name (makePayload payload)
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    makePayload o = object [ "old" .= Null, "new" .= o ]

    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _         -> throw400 NotSupported "manual mode is not enabled for event trigger"

getEventTriggerDef
  :: TriggerName
  -> Q.TxE QErr (PG.QualifiedTable, EventTriggerConf)
getEventTriggerDef triggerName = do
  (sn, tn, Q.AltJ etc) <- Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT e.schema_name, e.table_name, e.configuration::json
     FROM hdb_catalog.event_triggers e where e.name = $1
           |] (Identity triggerName) False
  return (PG.QualifiedObject sn tn, etc)

askTabInfoFromTrigger
  :: (QErrM m, CacheRM m)
  => SourceName -> TriggerName -> m (TableInfo ('Postgres 'Vanilla))
askTabInfoFromTrigger sourceName trn = do
  sc <- askSchemaCache
  let tabInfos = HM.elems $ fromMaybe mempty $ unsafeTableCache sourceName $ scSources sc
  find (isJust . HM.lookup trn . _tiEventTriggerInfoMap) tabInfos
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"

askEventTriggerInfo
  :: (QErrM m, CacheRM m)
  => SourceName -> TriggerName -> m EventTriggerInfo
askEventTriggerInfo sourceName trn = do
  ti <- askTabInfoFromTrigger sourceName trn
  let etim = _tiEventTriggerInfoMap ti
  HM.lookup trn etim `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"


-- This change helps us create functions for the event triggers
-- without the function name being truncated by PG, since PG allows
-- for only 63 chars for identifiers.
-- Reasoning for the 42 characters:
-- 63 - (notify_hasura_) - (_INSERT | _UPDATE | _DELETE)
maxTriggerNameLength :: Int
maxTriggerNameLength = 42
