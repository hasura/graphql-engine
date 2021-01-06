module Hasura.RQL.DDL.EventTrigger
  ( CreateEventTriggerQuery
  , runCreateEventTriggerQuery
  , DeleteEventTriggerQuery
  , runDeleteEventTriggerQuery
  , dropEventTriggerInMetadata
  , RedeliverEventQuery
  , runRedeliverEvent
  , runInvokeEventTrigger
  , createPostgresTableEventTrigger

  -- TODO(from master): review
  , mkEventTriggerInfo
  , mkAllTriggersQ
  , delTriggerQ
  , archiveEvents
  , getEventTriggerDef
  , getWebhookInfoFromConf
  , getHeaderInfosFromConf
  ) where

import           Hasura.Prelude

import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as HM
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as TL
import qualified Database.PG.Query                      as Q
import qualified Text.Shakespeare.Text                  as ST

import           Control.Lens                           ((.~))
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML       as S

import           Hasura.Backends.Postgres.Execute.Types
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types


data OpVar = OLD | NEW deriving (Show)

-- pgIdenTrigger is a method used to construct the name of the pg function
-- used for event triggers which are present in the hdb_catalog schema.
pgIdenTrigger:: Ops -> TriggerName -> Text
pgIdenTrigger op trn = pgFmtIdentifier . qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> tshow op'

mkAllTriggersQ
  :: (MonadTx m, HasSQLGenCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> TriggerOpsDef
  -> m ()
mkAllTriggersQ trn qt allCols fullspec = do
  onJust (tdInsert fullspec) (mkTriggerQ trn qt allCols INSERT)
  onJust (tdUpdate fullspec) (mkTriggerQ trn qt allCols UPDATE)
  onJust (tdDelete fullspec) (mkTriggerQ trn qt allCols DELETE)

mkTriggerQ
  :: (MonadTx m, HasSQLGenCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> Ops
  -> SubscribeOpSpec
  -> m ()
mkTriggerQ trn qt@(QualifiedObject schema table) allCols op (SubscribeOpSpec columns payload) = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  liftTx $ Q.multiQE defaultTxErrorHandler $ Q.fromText . TL.toStrict $
    let payloadColumns = fromMaybe SubCStar payload
        mkQId opVar colInfo = toJSONableExp strfyNum (pgiType colInfo) False $
          S.SEQIdentifier $ S.QIdentifier (opToQual opVar) $ toIdentifier $ pgiColumn colInfo
        getRowExpression opVar = case payloadColumns of
          SubCStar -> applyRowToJson $ S.SEUnsafe $ opToTxt opVar
          SubCArray cols -> applyRowToJson $
            S.mkRowExp $ map (toExtr . mkQId opVar) $
            getColInfos cols allCols

        renderRow opVar = case columns of
          SubCStar -> applyRow $ S.SEUnsafe $ opToTxt opVar
          SubCArray cols -> applyRow $
            S.mkRowExp $ map (toExtr . mkQId opVar) $
            getColInfos cols allCols

        oldDataExp = case op of
          INSERT -> S.SENull
          UPDATE -> getRowExpression OLD
          DELETE -> getRowExpression OLD
          MANUAL -> S.SENull
        newDataExp = case op of
          INSERT -> getRowExpression NEW
          UPDATE -> getRowExpression NEW
          DELETE -> S.SENull
          MANUAL -> S.SENull

        name = triggerNameToTxt trn
        qualifiedTriggerName = pgIdenTrigger op trn
        qualifiedTable = toSQLTxt qt
        schemaName = pgFmtLit $ getSchemaTxt schema
        tableName  = pgFmtLit $ getTableTxt table

        operation = tshow op
        oldRow = toSQLTxt $ renderRow OLD
        newRow = toSQLTxt $ renderRow NEW
        oldPayloadExpression = toSQLTxt oldDataExp
        newPayloadExpression = toSQLTxt newDataExp

    in $(ST.stextFile "src-rsr/trigger.sql.shakespeare")
  where
    applyRowToJson e = S.SEFnApp "row_to_json" [e] Nothing
    applyRow e = S.SEFnApp "row" [e] Nothing
    toExtr = flip S.Extractor Nothing
    opToQual = S.QualVar . opToTxt
    opToTxt = tshow

delTriggerQ :: TriggerName -> Q.TxE QErr ()
delTriggerQ trn =
  mapM_ (\op -> Q.unitQE
                defaultTxErrorHandler
          (Q.fromText $ getDropFuncSql op) () False) [INSERT, UPDATE, DELETE]
  where
    getDropFuncSql :: Ops -> T.Text
    getDropFuncSql op =
      "DROP FUNCTION IF EXISTS"
      <> " hdb_catalog." <> pgIdenTrigger op trn <> "()"
      <> " CASCADE"

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
    assertCols _ Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols (assertPGCol (_tciFieldInfoMap ti) "")

mkEventTriggerInfo
  :: QErrM m
  => Env.Environment
  -> SourceName
  -> QualifiedTable
  -> EventTriggerConf
  -> m (EventTriggerInfo, [SchemaDependency])
mkEventTriggerInfo env source qt (EventTriggerConf name def webhook webhookFromEnv rconf mheaders) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing)    -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _                    -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf env webhookConf
  headerInfos <- getHeaderInfosFromConf env headerConfs
  let eTrigInfo = EventTriggerInfo name def rconf webhookInfo headerInfos
      tabDep = SchemaDependency (SOSourceObj source $ SOITable qt) DRParent
  pure (eTrigInfo, tabDep:getTrigDefDeps source qt def)

getTrigDefDeps :: SourceName -> QualifiedTable -> TriggerOpsDef -> [SchemaDependency]
getTrigDefDeps source qt (TriggerOpsDef mIns mUpd mDel _) =
  mconcat $ catMaybes [ subsOpSpecDeps <$> mIns
                      , subsOpSpecDeps <$> mUpd
                      , subsOpSpecDeps <$> mDel
                      ]
  where
    subsOpSpecDeps :: SubscribeOpSpec -> [SchemaDependency]
    subsOpSpecDeps os =
      let cols = getColsFromSub $ sosColumns os
          colDeps = flip map cols $ \col ->
            SchemaDependency (SOSourceObj source $ SOITableObj qt (TOCol col)) DRColumn
          payload = maybe [] getColsFromSub (sosPayload os)
          payloadDeps = flip map payload $ \col ->
            SchemaDependency (SOSourceObj source $ SOITableObj qt (TOCol col)) DRPayload
        in colDeps <> payloadDeps
    getColsFromSub sc = case sc of
      SubCStar         -> []
      SubCArray pgcols -> pgcols

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

-- | Create the table event trigger in the database in a @'/v1/query' API
-- transaction as soon as after @'runCreateEventTriggerQuery' is called and
-- in building schema cache.
createPostgresTableEventTrigger
  :: (MonadTx m, HasSQLGenCtx m)
  => QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> TriggerName
  -> TriggerOpsDef
  -> m ()
createPostgresTableEventTrigger table columns triggerName opsDefinition = do
  -- Clean all existing triggers
  liftTx $ delTriggerQ triggerName -- executes DROP IF EXISTS.. sql
  -- Create the given triggers
  mkAllTriggersQ triggerName table columns opsDefinition

runDeleteEventTriggerQuery
  :: (MonadError QErr m, CacheRWM m, MonadIO m, MetadataM m)
  => DeleteEventTriggerQuery -> m EncJSON
runDeleteEventTriggerQuery (DeleteEventTriggerQuery source name) = do
  -- liftTx $ delEventTriggerFromCatalog name
  SourceInfo _ tables _ sourceConfig <- askPGSourceCache source
  let maybeTable = HM.lookup name $ HM.unions $
                   flip map (HM.toList tables) $ \(table, tableInfo) ->
                   HM.map (const table) $ _tiEventTriggerInfoMap tableInfo
  table <- onNothing maybeTable $ throw400 NotExists $
           "event trigger with name " <> name <<> " not exists"

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ tableMetadataSetter source table %~ dropEventTriggerInMetadata name

  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $ do
    delTriggerQ name
    archiveEvents name
  pure successMsg

dropEventTriggerInMetadata :: TriggerName -> TableMetadata -> TableMetadata
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
  sourceConfig <- _pcConfiguration <$> askPGSourceCache source
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
  sourceConfig <- _pcConfiguration <$> askPGSourceCache source
  eid <- liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $
         insertManualEvent (_tciName $ _tiCoreInfo ti) name payload
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _         -> throw400 NotSupported "manual mode is not enabled for event trigger"

getHeaderInfosFromConf
  :: QErrM m
  => Env.Environment
  -> [HeaderConf]
  -> m [EventHeaderInfo]
getHeaderInfosFromConf env = mapM getHeader
  where
    getHeader :: QErrM m => HeaderConf -> m EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> return $ EventHeaderInfo hconf val
      (HeaderConf _ (HVEnv val))   -> do
        envVal <- getEnv env val
        return $ EventHeaderInfo hconf envVal

getWebhookInfoFromConf
  :: QErrM m
  => Env.Environment
  -> WebhookConf
  -> m WebhookConfInfo
getWebhookInfoFromConf env wc = case wc of
  WCValue w -> do
    resolvedWebhook <- resolveWebhook env w
    return $ WebhookConfInfo wc $ unResolvedWebhook resolvedWebhook
  WCEnv we -> do
    envVal <- getEnv env we
    return $ WebhookConfInfo wc envVal

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
