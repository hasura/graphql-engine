{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.DDL.EventTrigger
  ( CreateEventTriggerQuery,
    runCreateEventTriggerQuery,
    DeleteEventTriggerQuery,
    runDeleteEventTriggerQuery,
    dropEventTriggerInMetadata,
    RedeliverEventQuery,
    runRedeliverEvent,
    InvokeEventTriggerQuery,
    runInvokeEventTrigger,
    -- TODO(from master): review
    ResolveHeaderError (..),
    getHeaderInfosFromConf,
    getHeaderInfosFromConfEither,
    getWebhookInfoFromConf,
    buildEventTriggerInfo,
    getSourceTableAndTriggers,
    getTriggerNames,
    getTriggersMap,
    getTableNameFromTrigger,
    cetqSource,
    cetqName,
    cetqTable,
    cetqInsert,
    cetqUpdate,
    cetqDelete,
    cetqEnableManual,
    cetqRetryConf,
    cetqWebhook,
    cetqWebhookFromEnv,
    cetqHeaders,
    cetqReplace,
    cetqRequestTransform,
    cetqResponseTrasnform,
    cteqCleanupConfig,
    cteqTriggerOnReplication,
    runCleanupEventTriggerLog,
    runEventTriggerResumeCleanup,
    runEventTriggerPauseCleanup,
    MonadEventLogCleanup (..),
    getAllEventTriggersWithCleanupConfig,
    getAllETWithCleanupConfigInTableMetadata,
    runGetEventLogs,
    runGetEventInvocationLogs,
    runGetEventById,
  )
where

import Control.Lens (ifor_, makeLenses, (.~))
import Data.Aeson
import Data.Either.Combinators
import Data.Environment qualified as Env
import Data.Has (Has)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Data.URL.Template (printTemplate, renderTemplate)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Backend
import Hasura.Eventing.EventTrigger (logQErr)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Headers (HeaderValue (..))
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session
import Hasura.Table.Cache
import Hasura.Table.Metadata (TableMetadata (..), tmEventTriggers)
import Hasura.Tracing (TraceT)
import Hasura.Tracing qualified as Tracing

data CreateEventTriggerQuery (b :: BackendType) = CreateEventTriggerQuery
  { _cetqSource :: SourceName,
    _cetqName :: TriggerName,
    _cetqTable :: TableName b,
    _cetqInsert :: Maybe (SubscribeOpSpec b),
    _cetqUpdate :: Maybe (SubscribeOpSpec b),
    _cetqDelete :: Maybe (SubscribeOpSpec b),
    _cetqEnableManual :: Maybe Bool,
    _cetqRetryConf :: Maybe RetryConf,
    _cetqWebhook :: Maybe InputWebhook,
    _cetqWebhookFromEnv :: Maybe Text,
    _cetqHeaders :: Maybe [HeaderConf],
    _cetqReplace :: Bool,
    _cetqRequestTransform :: Maybe RequestTransform,
    _cetqResponseTrasnform :: Maybe MetadataResponseTransform,
    _cteqCleanupConfig :: Maybe AutoTriggerLogCleanupConfig,
    _cteqTriggerOnReplication :: TriggerOnReplication
  }

$(makeLenses ''CreateEventTriggerQuery)

instance (Backend b) => FromJSON (CreateEventTriggerQuery b) where
  parseJSON = withObject "CreateEventTriggerQuery" \o -> do
    sourceName <- o .:? "source" .!= defaultSource
    name <- o .: "name"
    table <- o .: "table"
    insert <- o .:? "insert"
    update <- o .:? "update"
    delete <- o .:? "delete"
    enableManual <- o .:? "enable_manual" .!= False
    retryConf <- o .:? "retry_conf"
    webhook <- o .:? "webhook"
    webhookFromEnv <- o .:? "webhook_from_env"
    headers <- o .:? "headers"
    replace <- o .:? "replace" .!= False
    requestTransform <- o .:? "request_transform"
    responseTransform <- o .:? "response_transform"
    cleanupConfig <- o .:? "cleanup_config"
    when (isIllegalTriggerName name)
      $ fail "only alphanumeric and underscore and hyphens allowed for name"
    unless (T.length (triggerNameToTxt name) <= maxTriggerNameLength)
      $ fail "event trigger name can be at most 42 characters"
    unless (any isJust [insert, update, delete] || enableManual)
      $ fail "atleast one amongst insert/update/delete/enable_manual spec must be provided"
    case (webhook, webhookFromEnv) of
      (Just _, Nothing) -> return ()
      (Nothing, Just _) -> return ()
      (Just _, Just _) -> fail "only one of webhook or webhook_from_env should be given"
      _ -> fail "must provide webhook or webhook_from_env"
    mapM_ checkEmptyCols [insert, update, delete]
    defTOR <- case defaultTriggerOnReplication @b of
      Just (_, dt) -> pure dt
      Nothing -> fail "No default setting for trigger_on_replication is defined for backend type."
    triggerOnReplication <- o .:? "trigger_on_replication" .!= defTOR
    return $ CreateEventTriggerQuery sourceName name table insert update delete (Just enableManual) retryConf webhook webhookFromEnv headers replace requestTransform responseTransform cleanupConfig triggerOnReplication
    where
      checkEmptyCols spec =
        case spec of
          Just (SubscribeOpSpec (SubCArray cols) _) -> when (null cols) (fail "found empty column specification")
          Just (SubscribeOpSpec _ (Just (SubCArray cols))) -> when (null cols) (fail "found empty payload specification")
          _ -> return ()

data DeleteEventTriggerQuery (b :: BackendType) = DeleteEventTriggerQuery
  { _detqSource :: SourceName,
    _detqName :: TriggerName
  }

instance FromJSON (DeleteEventTriggerQuery b) where
  parseJSON = withObject "DeleteEventTriggerQuery" $ \o ->
    DeleteEventTriggerQuery
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "name"

data RedeliverEventQuery (b :: BackendType) = RedeliverEventQuery
  { _rdeqEventId :: EventId,
    _rdeqSource :: SourceName
  }

instance FromJSON (RedeliverEventQuery b) where
  parseJSON = withObject "RedeliverEventQuery" $ \o ->
    RedeliverEventQuery
      <$> o
      .: "event_id"
      <*> o
      .:? "source"
      .!= defaultSource

data InvokeEventTriggerQuery (b :: BackendType) = InvokeEventTriggerQuery
  { _ietqName :: TriggerName,
    _ietqSource :: SourceName,
    _ietqPayload :: Value
  }

instance (Backend b) => FromJSON (InvokeEventTriggerQuery b) where
  parseJSON = withObject "InvokeEventTriggerQuery" $ \o ->
    InvokeEventTriggerQuery
      <$> o
      .: "name"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "payload"

-- | This typeclass have the implementation logic for the event trigger log cleanup.
--
-- TODO: this doesn't belong here in the DDL folder, but should be part of
-- Hasura.Eventing. It could even be made a Service, since the whole point of it
-- is to implement features differently between OSS and Pro.
class (Monad m) => MonadEventLogCleanup m where
  -- Deletes the logs of event triggers
  runLogCleaner ::
    SourceCache -> TriggerLogCleanupConfig -> m (Either QErr EncJSON)

  -- Generates the cleanup schedules for event triggers which have log cleaners installed
  generateCleanupSchedules ::
    AB.AnyBackend SourceInfo -> TriggerName -> AutoTriggerLogCleanupConfig -> m (Either QErr ())

  -- | `updateTriggerCleanupSchedules` is primarily used to update the
  --    cleanup schedules associated with an event trigger in case the cleanup
  --    config has changed while replacing the metadata.
  --
  --    In case,
  --    i. a source has been dropped -
  --           We don't need to clear the cleanup schedules
  --           because the event log cleanup table is dropped as part
  --           of the post drop source hook.
  --    ii. a table or an event trigger has been dropped/updated -
  --           Older cleanup events will be deleted first and in case of
  --           an update, new cleanup events will be generated and inserted
  --           into the table.
  --    iii. a new event trigger with cleanup config has been added -
  --             Generate the cleanup events and insert it.
  --    iv. a new source has been added -
  --           Generate the cleanup events and insert it.
  --    v. the cron schedule for event trigger cleanup config has changed -
  --           Delete cleanup events with older cron schedule and generate
  --           cleanup events with new cron schedule.
  updateTriggerCleanupSchedules ::
    L.Logger L.Hasura ->
    InsOrdHashMap SourceName BackendSourceMetadata ->
    InsOrdHashMap SourceName BackendSourceMetadata ->
    SchemaCache ->
    m (Either QErr ())

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (ReaderT r m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (ExceptT e m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (MetadataT m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (TraceT m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (StateT w m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

resolveEventTriggerQuery ::
  forall b m.
  (Backend b, UserInfoM m, QErrM m, CacheRM m) =>
  CreateEventTriggerQuery b ->
  m (Bool, EventTriggerConf b)
resolveEventTriggerQuery (CreateEventTriggerQuery source name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace reqTransform respTransform cleanupConfig triggerOnReplication) = do
  ti <- askTableCoreInfo source qt
  -- can only replace for same table
  when replace $ do
    ti' <- _tiCoreInfo <$> askTabInfoFromTrigger @b source name
    when (_tciName ti' /= _tciName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (replace, EventTriggerConf name (TriggerOpsDef insert update delete enableManual) webhook webhookFromEnv rconf mheaders reqTransform respTransform cleanupConfig triggerOnReplication)
  where
    assertCols :: TableCoreInfo b -> Maybe (SubscribeOpSpec b) -> m ()
    assertCols ti opSpec = for_ opSpec \sos -> case sosColumns sos of
      SubCStar -> return ()
      SubCArray columns -> forM_ columns (assertColumnExists @b (_tciFieldInfoMap ti) "")

droppedTriggerOps :: TriggerOpsDef b -> TriggerOpsDef b -> HashSet Ops
droppedTriggerOps oldEventTriggerOps newEventTriggerOps =
  Set.fromList
    $ catMaybes
    $ [ (bool Nothing (Just INSERT) (isDroppedOp (tdInsert oldEventTriggerOps) (tdInsert newEventTriggerOps))),
        (bool Nothing (Just UPDATE) (isDroppedOp (tdUpdate oldEventTriggerOps) (tdUpdate newEventTriggerOps))),
        (bool Nothing (Just DELETE) (isDroppedOp (tdDelete oldEventTriggerOps) (tdDelete newEventTriggerOps)))
      ]
  where
    isDroppedOp old new = isJust old && isNothing new

createEventTriggerQueryMetadata ::
  forall b m r.
  ( BackendMetadata b,
    QErrM m,
    UserInfoM m,
    CacheRWM m,
    MetadataM m,
    BackendEventTrigger b,
    Tracing.MonadTraceContext m,
    MonadIO m,
    MonadEventLogCleanup m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  CreateEventTriggerQuery b ->
  m ()
createEventTriggerQueryMetadata q = do
  (replace, triggerConf) <- resolveEventTriggerQuery q
  let table = _cetqTable q
      source = _cetqSource q
      triggerName = etcName triggerConf
      metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOTableObj @b table
          $ MTOTrigger triggerName
  sourceInfo <- askSourceInfo @b source
  let sourceConfig = (_siConfiguration sourceInfo)
      newConfig = _cteqCleanupConfig q

  -- Check for existence of a trigger with 'triggerName' only when 'replace' is not set
  if replace
    then do
      existingEventTriggerOps <- etiOpsDef <$> askEventTriggerInfo @b source triggerName
      let droppedOps = droppedTriggerOps existingEventTriggerOps (etcDefinition triggerConf)
      dropDanglingSQLTrigger @b (_siConfiguration sourceInfo) triggerName table droppedOps

      -- check if cron schedule for the cleanup config has changed then delete the scheduled cleanups
      oldConfig <- etiCleanupConfig <$> askEventTriggerInfo @b source triggerName
      when (hasCleanupCronScheduleUpdated oldConfig newConfig) do
        deleteAllScheduledCleanups @b sourceConfig triggerName
        for_ newConfig \cleanupConfig -> do
          (`onLeft` logQErr) =<< generateCleanupSchedules (AB.mkAnyBackend sourceInfo) triggerName cleanupConfig
    else do
      doesTriggerExists <- checkIfTriggerExists @b sourceConfig triggerName (Set.fromList [INSERT, UPDATE, DELETE])
      if doesTriggerExists
        then throw400 AlreadyExists ("Event trigger with name " <> triggerNameToTxt triggerName <<> " already exists")
        else for_ newConfig \cleanupConfig -> do
          (`onLeft` logQErr) =<< generateCleanupSchedules (AB.mkAnyBackend sourceInfo) triggerName cleanupConfig

  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b source table
    . tmEventTriggers
    %~ if replace
      then ix triggerName .~ triggerConf
      else InsOrdHashMap.insert triggerName triggerConf

runCreateEventTriggerQuery ::
  forall b m r.
  ( BackendMetadata b,
    BackendEventTrigger b,
    QErrM m,
    UserInfoM m,
    CacheRWM m,
    Tracing.MonadTraceContext m,
    MetadataM m,
    MonadIO m,
    MonadEventLogCleanup m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  CreateEventTriggerQuery b ->
  m EncJSON
runCreateEventTriggerQuery q = do
  createEventTriggerQueryMetadata @b q
  pure successMsg

runDeleteEventTriggerQuery ::
  forall b m.
  (BackendEventTrigger b, MonadError QErr m, CacheRWM m, MonadIO m, MetadataM m) =>
  DeleteEventTriggerQuery b ->
  m EncJSON
runDeleteEventTriggerQuery (DeleteEventTriggerQuery sourceName triggerName) = do
  sourceConfig <- askSourceConfig @b sourceName
  tableName <- (_tciName . _tiCoreInfo) <$> askTabInfoFromTrigger @b sourceName triggerName

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ tableMetadataSetter @b sourceName tableName
    %~ dropEventTriggerInMetadata triggerName

  dropTriggerAndArchiveEvents @b sourceConfig triggerName tableName

  deleteAllScheduledCleanups @b sourceConfig triggerName

  pure successMsg

runRedeliverEvent ::
  forall b m.
  (BackendEventTrigger b, MonadIO m, CacheRM m, QErrM m, MetadataM m) =>
  RedeliverEventQuery b ->
  m EncJSON
runRedeliverEvent (RedeliverEventQuery eventId source) = do
  sourceConfig <- askSourceConfig @b source
  redeliverEvent @b sourceConfig eventId
  pure successMsg

runInvokeEventTrigger ::
  forall b m.
  ( MonadIO m,
    QErrM m,
    CacheRM m,
    MetadataM m,
    Tracing.MonadTrace m,
    UserInfoM m,
    BackendEventTrigger b
  ) =>
  InvokeEventTriggerQuery b ->
  m EncJSON
runInvokeEventTrigger (InvokeEventTriggerQuery name source payload) = do
  trigInfo <- askEventTriggerInfo @b source name
  assertManual $ etiOpsDef trigInfo
  ti <- askTabInfoFromTrigger source name
  sourceConfig <- askSourceConfig @b source
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  eid <- insertManualEvent @b sourceConfig (tableInfoName @b ti) name (makePayload payload) userInfo traceCtx
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    makePayload o = object ["old" .= Null, "new" .= o]

    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _ -> throw400 NotSupported "manual mode is not enabled for event trigger"

askTabInfoFromTrigger ::
  (Backend b, QErrM m, CacheRM m) =>
  SourceName ->
  TriggerName ->
  m (TableInfo b)
askTabInfoFromTrigger sourceName triggerName = do
  schemaCache <- askSchemaCache
  getTabInfoFromSchemaCache schemaCache sourceName triggerName
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerName <<> " does not exist"

getTabInfoFromSchemaCache ::
  (Backend b) =>
  SchemaCache ->
  SourceName ->
  TriggerName ->
  Maybe (TableInfo b)
getTabInfoFromSchemaCache schemaCache sourceName triggerName = do
  tableCache <- unsafeTableCache sourceName $ scSources schemaCache
  find (isJust . HashMap.lookup triggerName . _tiEventTriggerInfoMap) (HashMap.elems tableCache)

askEventTriggerInfo ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TriggerName ->
  m (EventTriggerInfo b)
askEventTriggerInfo sourceName triggerName = do
  triggerInfo <- askTabInfoFromTrigger @b sourceName triggerName
  let eventTriggerInfoMap = _tiEventTriggerInfoMap triggerInfo
  HashMap.lookup triggerName eventTriggerInfoMap `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerName <<> " does not exist"

checkIfTriggerNameExists ::
  forall b m.
  (Backend b, CacheRM m) =>
  SourceName ->
  TriggerName ->
  m (Bool)
checkIfTriggerNameExists sourceName triggerName = do
  schemaCache <- askSchemaCache
  -- TODO: The name getTabInfoFromSchemaCache is misleading here.
  -- There is a JIRA ticket that addresses this (https://hasurahq.atlassian.net/browse/GS-535)
  let tableInfoMaybe = getTabInfoFromSchemaCache @b schemaCache sourceName triggerName
  case tableInfoMaybe of
    Nothing -> pure False
    _ -> pure True

-- This change helps us create functions for the event triggers
-- without the function name being truncated by PG, since PG allows
-- for only 63 chars for identifiers.
-- Reasoning for the 42 characters:
-- 63 - (notify_hasura_) - (_INSERT | _UPDATE | _DELETE)
maxTriggerNameLength :: Int
maxTriggerNameLength = 42

-- Consists of a list of environment variables with invalid/missing values
newtype ResolveHeaderError = ResolveHeaderError {unResolveHeaderError :: [Text]} deriving (Show)

instance ToTxt ResolveHeaderError where
  toTxt = commaSeparated . unResolveHeaderError

getHeaderInfosFromConf ::
  (QErrM m) =>
  Env.Environment ->
  [HeaderConf] ->
  m [EventHeaderInfo]
getHeaderInfosFromConf env = mapM getHeader
  where
    getHeader :: (QErrM m) => HeaderConf -> m EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> case renderTemplate env val of
        Left err -> throw400 NotFound $ "template cannot be resolved: " <> err
        Right resolvedVal -> return $ EventHeaderInfo hconf resolvedVal
      (HeaderConf _ (HVEnv val)) -> do
        envVal <- getEnv env val
        return $ EventHeaderInfo hconf envVal

-- This is similar to `getHeaderInfosFromConf` but it doesn't fail when an env var is invalid
getHeaderInfosFromConfEither ::
  Env.Environment ->
  [HeaderConf] ->
  Either ResolveHeaderError [EventHeaderInfo]
getHeaderInfosFromConfEither env hConfList =
  if isHeaderError
    then Left (ResolveHeaderError $ lefts headerInfoList)
    else Right (rights headerInfoList)
  where
    isHeaderError = any isLeft headerInfoList
    headerInfoList = map getHeader hConfList
    getHeader :: HeaderConf -> Either Text EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> case renderTemplate env val of
        Left err -> Left $ "template cannot be resolved: " <> tshow err
        Right resolvedVal -> Right $ EventHeaderInfo hconf resolvedVal
      (HeaderConf _ (HVEnv val)) ->
        (Right . EventHeaderInfo hconf) =<< getEnvEither env val

getWebhookInfoFromConf ::
  (QErrM m) =>
  Env.Environment ->
  WebhookConf ->
  m WebhookConfInfo
getWebhookInfoFromConf env webhookConf = case webhookConf of
  WCValue w -> do
    resolvedWebhook <- resolveWebhook env w
    let urlTemplate = printTemplate $ unInputWebhook w
    -- `urlTemplate` can either be the template value({{TEST}}) or a plain text.
    -- When `urlTemplate` is a template value then '_envVarName' of the 'EnvRecord'
    -- will be the template value i.e '{{TEST}}'
    -- When `urlTemplate` is a plain text then '_envVarName' of the 'EnvRecord'  will be the plain text value.
    return $ WebhookConfInfo webhookConf (EnvRecord urlTemplate resolvedWebhook)
  WCEnv webhookEnvVar -> do
    envVal <- getEnv env webhookEnvVar
    return $ WebhookConfInfo webhookConf (EnvRecord webhookEnvVar (ResolvedWebhook envVal))

buildEventTriggerInfo ::
  forall b m.
  (Backend b, QErrM m) =>
  Env.Environment ->
  SourceName ->
  TableName b ->
  EventTriggerConf b ->
  m (EventTriggerInfo b, Seq SchemaDependency)
buildEventTriggerInfo
  env
  source
  tableName
  ( EventTriggerConf
      name
      def
      webhook
      webhookFromEnv
      rconf
      mheaders
      reqTransform
      respTransform
      cleanupConfig
      triggerOnReplication
    ) = do
    webhookConf <- case (webhook, webhookFromEnv) of
      (Just w, Nothing) -> return $ WCValue w
      (Nothing, Just wEnv) -> return $ WCEnv wEnv
      _ -> throw500 "expected webhook or webhook_from_env"
    let headerConfs = fromMaybe [] mheaders
    webhookInfo <- getWebhookInfoFromConf env webhookConf
    headerInfos <- getHeaderInfosFromConf env headerConfs
    let eTrigInfo =
          EventTriggerInfo
            name
            def
            rconf
            webhookInfo
            headerInfos
            reqTransform
            respTransform
            cleanupConfig
            triggerOnReplication
        tabDep =
          SchemaDependency
            ( SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITable @b tableName
            )
            DRParent
    pure (eTrigInfo, tabDep Seq.:<| getTrigDefDeps @b source tableName def)

getTrigDefDeps ::
  forall b.
  (Backend b) =>
  SourceName ->
  TableName b ->
  TriggerOpsDef b ->
  Seq SchemaDependency
getTrigDefDeps source tableName (TriggerOpsDef mIns mUpd mDel _) =
  mconcat
    $ Seq.fromList
    <$> catMaybes
      [ subsOpSpecDeps <$> mIns,
        subsOpSpecDeps <$> mUpd,
        subsOpSpecDeps <$> mDel
      ]
  where
    subsOpSpecDeps :: SubscribeOpSpec b -> [SchemaDependency]
    subsOpSpecDeps os =
      let cols = getColsFromSub $ sosColumns os
          mkColDependency dependencyReason col =
            SchemaDependency
              ( SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b tableName (TOCol @b col)
              )
              dependencyReason
          colDeps = map (mkColDependency DRColumn) cols
          payload = maybe [] getColsFromSub (sosPayload os)
          payloadDeps = map (mkColDependency DRPayload) payload
       in colDeps <> payloadDeps
    getColsFromSub sc = case sc of
      SubCStar -> []
      SubCArray cols -> cols

getTriggersMap ::
  SourceMetadata b ->
  InsOrdHashMap TriggerName (EventTriggerConf b)
getTriggersMap = InsOrdHashMap.unions . map _tmEventTriggers . InsOrdHashMap.elems . _smTables

getSourceTableAndTriggers ::
  SourceMetadata b ->
  [(TableName b, TriggerName)]
getSourceTableAndTriggers =
  (concatMap mkKeyValue) . InsOrdHashMap.toList . _smTables
  where
    mkKeyValue (tableName, tableMetadata) = map (tableName,) $ InsOrdHashMap.keys (_tmEventTriggers tableMetadata)

getTriggerNames ::
  SourceMetadata b ->
  Set.HashSet TriggerName
getTriggerNames = Set.fromList . InsOrdHashMap.keys . getTriggersMap

getTableNameFromTrigger ::
  forall b.
  (Backend b) =>
  SchemaCache ->
  SourceName ->
  TriggerName ->
  Maybe (TableName b)
getTableNameFromTrigger schemaCache sourceName triggerName = do
  tableInfo <- getTabInfoFromSchemaCache @b schemaCache sourceName triggerName
  pure $ _tciName $ _tiCoreInfo tableInfo

runCleanupEventTriggerLog ::
  (MonadEventLogCleanup m, MonadError QErr m, CacheRWM m) =>
  TriggerLogCleanupConfig ->
  m EncJSON
runCleanupEventTriggerLog conf = do
  sourceCache <- scSources <$> askSchemaCache
  runLogCleaner sourceCache conf `onLeftM` throwError

-- | Updates the cleanup switch in metadata given the source, table and trigger name
-- The Bool value represents the status of the cleaner, whether to start or pause it
updateCleanupStatusInMetadata ::
  forall b m.
  (Backend b, QErrM m, CacheRWM m, MetadataM m) =>
  AutoTriggerLogCleanupConfig ->
  EventTriggerCleanupStatus ->
  SourceName ->
  TableName b ->
  TriggerName ->
  m ()
updateCleanupStatusInMetadata cleanupConfig cleanupSwitch sourceName tableName triggerName = do
  let newCleanupConfig = Just $ cleanupConfig {_atlccPaused = cleanupSwitch}
      metadataObj =
        MOSourceObjId sourceName
          $ AB.mkAnyBackend
          $ SMOTableObj @b tableName
          $ MTOTrigger triggerName

  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b sourceName tableName
    . tmEventTriggers
    . ix triggerName
    %~ updateCleanupConfig newCleanupConfig

-- | Function to start/stop the cleanup action based on the event triggers supplied in
-- TriggerLogCleanupToggleConfig conf
toggleEventTriggerCleanupAction ::
  forall m.
  (MonadIO m, QErrM m, CacheRWM m, MetadataM m) =>
  TriggerLogCleanupToggleConfig ->
  EventTriggerCleanupStatus ->
  m EncJSON
toggleEventTriggerCleanupAction conf cleanupSwitch = do
  schemaCache <- askSchemaCache
  case conf of
    TriggerLogCleanupSources tlcs -> do
      case tlcs of
        TriggerAllSource -> do
          ifor_ (scSources schemaCache) $ \sourceName backendSourceInfo -> do
            AB.dispatchAnyBackend @BackendEventTrigger backendSourceInfo \(SourceInfo {..} :: SourceInfo b) -> do
              traverseTableHelper _siTables cleanupSwitch sourceName
        TriggerSource sourceNameLst -> do
          forM_ sourceNameLst $ \sourceName -> do
            backendSourceInfo <-
              HashMap.lookup sourceName (scSources schemaCache)
                `onNothing` throw400 NotExists ("source with name " <> sourceNameToText sourceName <> " does not exists")

            AB.dispatchAnyBackend @BackendEventTrigger backendSourceInfo \(SourceInfo {..} :: SourceInfo b) -> do
              traverseTableHelper _siTables cleanupSwitch sourceName
    TriggerQualifier qualifierLst -> do
      forM_ qualifierLst $ \qualifier -> do
        let sourceName = _etqSourceName qualifier
            triggerNames = _etqEventTriggers qualifier

        backendSourceInfo <-
          HashMap.lookup sourceName (scSources schemaCache)
            `onNothing` throw400 NotExists ("source with name " <> sourceNameToText sourceName <> " does not exists")

        AB.dispatchAnyBackend @BackendEventTrigger backendSourceInfo \(SourceInfo {} :: SourceInfo b) -> do
          for_ triggerNames $ \triggerName -> do
            eventTriggerInfo <- askEventTriggerInfo @b sourceName triggerName
            tableName <-
              getTableNameFromTrigger @b schemaCache sourceName triggerName
                `onNothing` throw400 NotExists ("event trigger " <> triggerName <<> " does not exist")
            cleanupConfig <-
              etiCleanupConfig eventTriggerInfo
                `onNothing` throw400 NotExists ("cleanup config does not exist for " <> triggerNameToTxt triggerName)
            updateCleanupStatusInMetadata @b cleanupConfig cleanupSwitch sourceName tableName triggerName
  pure successMsg
  where
    traverseTableHelper ::
      forall b.
      (Backend b) =>
      TableCache b ->
      EventTriggerCleanupStatus ->
      SourceName ->
      m ()
    traverseTableHelper tableCache switch sourceName = forM_ tableCache $ \tableInfo -> do
      let tableName = (_tciName . _tiCoreInfo) tableInfo
          eventTriggerInfoMap = _tiEventTriggerInfoMap tableInfo
      ifor_ eventTriggerInfoMap $ \triggerName eventTriggerInfo -> do
        for_ (etiCleanupConfig eventTriggerInfo) $ \cleanupConfig ->
          updateCleanupStatusInMetadata @b cleanupConfig switch sourceName tableName triggerName

runEventTriggerResumeCleanup ::
  forall m.
  (MonadIO m, QErrM m, CacheRWM m, MetadataM m) =>
  TriggerLogCleanupToggleConfig ->
  m EncJSON
runEventTriggerResumeCleanup conf = toggleEventTriggerCleanupAction conf ETCSUnpaused

runEventTriggerPauseCleanup ::
  (MonadError QErr m, CacheRWM m, MonadIO m, MetadataM m) =>
  TriggerLogCleanupToggleConfig ->
  m EncJSON
runEventTriggerPauseCleanup conf = toggleEventTriggerCleanupAction conf ETCSPaused

-- | Collects and returns all the event triggers with cleanup config
getAllEventTriggersWithCleanupConfig :: TableInfo b -> [(TriggerName, AutoTriggerLogCleanupConfig)]
getAllEventTriggersWithCleanupConfig tInfo = mapMaybe (\(triggerName, triggerInfo) -> (triggerName,) <$> etiCleanupConfig triggerInfo) $ HashMap.toList $ _tiEventTriggerInfoMap tInfo

hasCleanupCronScheduleUpdated :: Maybe AutoTriggerLogCleanupConfig -> Maybe AutoTriggerLogCleanupConfig -> Bool
hasCleanupCronScheduleUpdated Nothing _ = False
hasCleanupCronScheduleUpdated _ Nothing = True
hasCleanupCronScheduleUpdated (Just oldConfig) (Just newConfig) =
  _atlccSchedule oldConfig /= _atlccSchedule newConfig

getAllETWithCleanupConfigInTableMetadata :: TableMetadata b -> [(TriggerName, AutoTriggerLogCleanupConfig)]
getAllETWithCleanupConfigInTableMetadata tMetadata =
  mapMaybe
    ( \(triggerName, triggerConf) ->
        (triggerName,)
          <$> etcCleanupConfig triggerConf
    )
    $ InsOrdHashMap.toList
    $ _tmEventTriggers tMetadata

runGetEventLogs ::
  forall b m.
  (MonadIO m, CacheRM m, MonadError QErr m, BackendEventTrigger b, MetadataM m) =>
  GetEventLogs b ->
  m EncJSON
runGetEventLogs getEventLogs = do
  sourceConfig <- askSourceConfig @b sourceName
  doesTriggerExists <- checkIfTriggerNameExists @b sourceName triggerName
  if not doesTriggerExists
    then throw400 NotExists $ "event trigger " <> triggerName <<> " does not exist"
    else encJFromJValue <$> fetchEventLogs sourceConfig getEventLogs
  where
    sourceName = _gelSourceName getEventLogs
    triggerName = _gelName getEventLogs

runGetEventInvocationLogs ::
  forall b m.
  (MonadIO m, CacheRM m, MonadError QErr m, BackendEventTrigger b, MetadataM m) =>
  GetEventInvocations b ->
  m EncJSON
runGetEventInvocationLogs getEventInvocations = do
  sourceConfig <- askSourceConfig @b sourceName
  doesTriggerExists <- checkIfTriggerNameExists @b sourceName triggerName
  if not doesTriggerExists
    then throw400 NotExists $ "event trigger " <> triggerName <<> " does not exist"
    else encJFromJValue <$> fetchEventInvocationLogs sourceConfig getEventInvocations
  where
    sourceName = _geiSourceName getEventInvocations
    triggerName = _geiName getEventInvocations

runGetEventById ::
  forall b m.
  (MonadIO m, CacheRM m, MonadError QErr m, BackendEventTrigger b, MetadataM m) =>
  GetEventById b ->
  m EncJSON
runGetEventById getEventById = do
  sourceConfig <- askSourceConfig @b sourceName
  encJFromJValue <$> fetchEventById sourceConfig getEventById
  where
    sourceName = _gebiSourceName getEventById
