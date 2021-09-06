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
import qualified Hasura.SQL.AnyBackend              as AB
import qualified Hasura.Tracing                     as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Eventing.Backend
import           Hasura.Session


data CreateEventTriggerQuery (b :: BackendType)
  = CreateEventTriggerQuery
  { _cetqSource         :: !SourceName
  , _cetqName           :: !TriggerName
  , _cetqTable          :: !(TableName b)
  , _cetqInsert         :: !(Maybe (SubscribeOpSpec b))
  , _cetqUpdate         :: !(Maybe (SubscribeOpSpec b))
  , _cetqDelete         :: !(Maybe (SubscribeOpSpec b))
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

resolveEventTriggerQuery
  :: forall b m
   . (Backend b, UserInfoM m, QErrM m, CacheRM m)
  => CreateEventTriggerQuery b
  -> m (TableCoreInfo b, Bool, EventTriggerConf b)
resolveEventTriggerQuery (CreateEventTriggerQuery source name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace) = do
  ti <- askTableCoreInfo source qt
  -- can only replace for same table
  when replace $ do
    ti' <- _tiCoreInfo <$> askTabInfoFromTrigger @b source name
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
  -> m (TableCoreInfo ('Postgres pgKind), EventTriggerConf ('Postgres pgKind))
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

runRedeliverEvent
  :: forall b m
   . (BackendEventTrigger b, MonadIO m, CacheRM m, QErrM m, MetadataM m)
  => RedeliverEventQuery b
  -> m EncJSON
runRedeliverEvent (RedeliverEventQuery eventId source) = do
  sourceConfig <- askSourceConfig @b source
  redeliverEvent @b sourceConfig eventId
  pure successMsg

runInvokeEventTrigger
  :: forall b m
   . ( MonadIO m
     , QErrM m
     , CacheRM m
     , MetadataM m
     , Tracing.MonadTrace m
     , UserInfoM m
     , BackendEventTrigger b
     )
  => InvokeEventTriggerQuery b
  -> m EncJSON
runInvokeEventTrigger (InvokeEventTriggerQuery name source payload) = do
  trigInfo <- askEventTriggerInfo @b source name
  assertManual $ etiOpsDef trigInfo
  ti  <- askTabInfoFromTrigger source name
  sourceConfig <- askSourceConfig @b source
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  eid <- insertManualEvent @b sourceConfig (tableInfoName @b ti) name (makePayload payload) userInfo traceCtx
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    makePayload o = object [ "old" .= Null, "new" .= o ]

    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _         -> throw400 NotSupported "manual mode is not enabled for event trigger"

askTabInfoFromTrigger
  :: (Backend b, QErrM m, CacheRM m)
  => SourceName -> TriggerName -> m (TableInfo b)
askTabInfoFromTrigger sourceName triggerName = do
  sc <- askSchemaCache
  let tabInfos = HM.elems $ fromMaybe mempty $ unsafeTableCache sourceName $ scSources sc
  find (isJust . HM.lookup triggerName . _tiEventTriggerInfoMap) tabInfos
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerName <<> " does not exist"

askEventTriggerInfo
  :: forall b m. (QErrM m, CacheRM m, Backend b)
  => SourceName -> TriggerName -> m (EventTriggerInfo b)
askEventTriggerInfo sourceName triggerName = do
  triggerInfo <- askTabInfoFromTrigger @b sourceName triggerName
  let eventTriggerInfoMap = _tiEventTriggerInfoMap triggerInfo
  HM.lookup triggerName eventTriggerInfoMap `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerName <<> " does not exist"

-- This change helps us create functions for the event triggers
-- without the function name being truncated by PG, since PG allows
-- for only 63 chars for identifiers.
-- Reasoning for the 42 characters:
-- 63 - (notify_hasura_) - (_INSERT | _UPDATE | _DELETE)
maxTriggerNameLength :: Int
maxTriggerNameLength = 42
