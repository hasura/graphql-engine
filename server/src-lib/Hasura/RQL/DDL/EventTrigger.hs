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
    getHeaderInfosFromConf,
    getWebhookInfoFromConf,
    buildEventTriggerInfo,
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
  )
where

import Control.Lens (makeLenses, (.~))
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Extended
import Data.URL.Template (printURLTemplate)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Eventing.Backend
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Text.Regex.TDFA qualified as TDFA

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
    _cetqResponseTrasnform :: Maybe MetadataResponseTransform
  }

$(makeLenses ''CreateEventTriggerQuery)

instance Backend b => FromJSON (CreateEventTriggerQuery b) where
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
      (Just _, Just _) -> fail "only one of webhook or webhook_from_env should be given"
      _ -> fail "must provide webhook or webhook_from_env"
    mapM_ checkEmptyCols [insert, update, delete]
    return $ CreateEventTriggerQuery sourceName name table insert update delete (Just enableManual) retryConf webhook webhookFromEnv headers replace requestTransform responseTransform
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
      <$> o .:? "source" .!= defaultSource
      <*> o .: "name"

data RedeliverEventQuery (b :: BackendType) = RedeliverEventQuery
  { _rdeqEventId :: EventId,
    _rdeqSource :: SourceName
  }

instance FromJSON (RedeliverEventQuery b) where
  parseJSON = withObject "RedeliverEventQuery" $ \o ->
    RedeliverEventQuery
      <$> o .: "event_id"
      <*> o .:? "source" .!= defaultSource

data InvokeEventTriggerQuery (b :: BackendType) = InvokeEventTriggerQuery
  { _ietqName :: TriggerName,
    _ietqSource :: SourceName,
    _ietqPayload :: Value
  }

instance Backend b => FromJSON (InvokeEventTriggerQuery b) where
  parseJSON = withObject "InvokeEventTriggerQuery" $ \o ->
    InvokeEventTriggerQuery
      <$> o .: "name"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "payload"

resolveEventTriggerQuery ::
  forall b m.
  (Backend b, UserInfoM m, QErrM m, CacheRM m) =>
  CreateEventTriggerQuery b ->
  m (Bool, EventTriggerConf b)
resolveEventTriggerQuery (CreateEventTriggerQuery source name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace reqTransform respTransform) = do
  ti <- askTableCoreInfo source qt
  -- can only replace for same table
  when replace $ do
    ti' <- _tiCoreInfo <$> askTabInfoFromTrigger @b source name
    when (_tciName ti' /= _tciName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (replace, EventTriggerConf name (TriggerOpsDef insert update delete enableManual) webhook webhookFromEnv rconf mheaders reqTransform respTransform)
  where
    assertCols :: TableCoreInfo b -> Maybe (SubscribeOpSpec b) -> m ()
    assertCols ti opSpec = onJust opSpec \sos -> case sosColumns sos of
      SubCStar -> return ()
      SubCArray columns -> forM_ columns (assertColumnExists @b (_tciFieldInfoMap ti) "")

droppedTriggerOps :: TriggerOpsDef b -> TriggerOpsDef b -> HashSet Ops
droppedTriggerOps oldEventTriggerOps newEventTriggerOps =
  Set.fromList $
    catMaybes $
      [ (bool Nothing (Just INSERT) (isDroppedOp (tdInsert oldEventTriggerOps) (tdInsert newEventTriggerOps))),
        (bool Nothing (Just UPDATE) (isDroppedOp (tdUpdate oldEventTriggerOps) (tdUpdate newEventTriggerOps))),
        (bool Nothing (Just DELETE) (isDroppedOp (tdDelete oldEventTriggerOps) (tdDelete newEventTriggerOps)))
      ]
  where
    isDroppedOp old new = isJust old && isNothing new

createEventTriggerQueryMetadata ::
  forall b m.
  (BackendMetadata b, QErrM m, UserInfoM m, CacheRWM m, MetadataM m, BackendEventTrigger b, MonadIO m) =>
  CreateEventTriggerQuery b ->
  m ()
createEventTriggerQueryMetadata q = do
  (replace, triggerConf) <- resolveEventTriggerQuery q
  let table = _cetqTable q
      source = _cetqSource q
      triggerName = etcName triggerConf
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTOTrigger triggerName
  sourceInfo <- askSourceInfo @b source
  when replace $ do
    existingEventTriggerOps <- etiOpsDef <$> askEventTriggerInfo @b source triggerName
    let droppedOps = droppedTriggerOps existingEventTriggerOps (etcDefinition triggerConf)
    dropDanglingSQLTrigger @b (_siConfiguration sourceInfo) triggerName table droppedOps

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b source table . tmEventTriggers
        %~ if replace
          then ix triggerName .~ triggerConf
          else OMap.insert triggerName triggerConf

runCreateEventTriggerQuery ::
  forall b m.
  (BackendMetadata b, BackendEventTrigger b, QErrM m, UserInfoM m, CacheRWM m, MetadataM m, MonadIO m) =>
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

  withNewInconsistentObjsCheck $
    buildSchemaCache $
      MetadataModifier $
        tableMetadataSetter @b sourceName tableName %~ dropEventTriggerInMetadata triggerName

  dropTriggerAndArchiveEvents @b sourceConfig triggerName tableName

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

getTabInfoFromSchemaCache ::
  (Backend b, QErrM m) =>
  SchemaCache ->
  SourceName ->
  TriggerName ->
  m (TableInfo b)
getTabInfoFromSchemaCache schemaCache sourceName triggerName = do
  let tabInfos = HM.elems $ fromMaybe mempty $ unsafeTableCache sourceName $ scSources schemaCache
  find (isJust . HM.lookup triggerName . _tiEventTriggerInfoMap) tabInfos
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "event trigger " <> triggerName <<> " does not exist"

askEventTriggerInfo ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TriggerName ->
  m (EventTriggerInfo b)
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

getHeaderInfosFromConf ::
  QErrM m =>
  Env.Environment ->
  [HeaderConf] ->
  m [EventHeaderInfo]
getHeaderInfosFromConf env = mapM getHeader
  where
    getHeader :: QErrM m => HeaderConf -> m EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> return $ EventHeaderInfo hconf val
      (HeaderConf _ (HVEnv val)) -> do
        envVal <- getEnv env val
        return $ EventHeaderInfo hconf envVal

getWebhookInfoFromConf ::
  QErrM m =>
  Env.Environment ->
  WebhookConf ->
  m WebhookConfInfo
getWebhookInfoFromConf env webhookConf = case webhookConf of
  WCValue w -> do
    resolvedWebhook <- resolveWebhook env w
    let urlTemplate = printURLTemplate $ unInputWebhook w
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
  m (EventTriggerInfo b, [SchemaDependency])
buildEventTriggerInfo env source tableName (EventTriggerConf name def webhook webhookFromEnv rconf mheaders reqTransform respTransform) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing) -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _ -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf env webhookConf
  headerInfos <- getHeaderInfosFromConf env headerConfs
  let eTrigInfo = EventTriggerInfo name def rconf webhookInfo headerInfos reqTransform respTransform
      tabDep =
        SchemaDependency
          ( SOSourceObj source $
              AB.mkAnyBackend $
                SOITable @b tableName
          )
          DRParent
  pure (eTrigInfo, tabDep : getTrigDefDeps @b source tableName def)

getTrigDefDeps ::
  forall b.
  Backend b =>
  SourceName ->
  TableName b ->
  TriggerOpsDef b ->
  [SchemaDependency]
getTrigDefDeps source tableName (TriggerOpsDef mIns mUpd mDel _) =
  mconcat $
    catMaybes
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
              ( SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITableObj @b tableName (TOCol @b col)
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
getTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _smTables

getTriggerNames ::
  SourceMetadata b ->
  Set.HashSet TriggerName
getTriggerNames = Set.fromList . OMap.keys . getTriggersMap

getTableNameFromTrigger ::
  forall b m.
  (Backend b, QErrM m) =>
  SchemaCache ->
  SourceName ->
  TriggerName ->
  m (TableName b)
getTableNameFromTrigger schemaCache sourceName triggerName =
  (_tciName . _tiCoreInfo) <$> getTabInfoFromSchemaCache @b schemaCache sourceName triggerName
