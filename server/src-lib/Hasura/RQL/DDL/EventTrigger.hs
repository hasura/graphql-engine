module Hasura.RQL.DDL.EventTrigger
  ( CreateEventTriggerQuery
  , runCreateEventTriggerQuery
  , DeleteEventTriggerQuery
  , runDeleteEventTriggerQuery
  , RedeliverEventQuery
  , runRedeliverEvent
  , runInvokeEventTrigger

  -- TODO: review
  , delEventTriggerFromCatalog
  , subTableP2
  , subTableP2Setup
  , mkAllTriggersQ
  , delTriggerQ
  , getEventTriggerDef
  , getWebhookInfoFromConf
  , getHeaderInfosFromConf
  , updateEventTriggerInCatalog
  ) where

import           Data.Aeson
import           System.Environment      (lookupEnv)

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML          as S

import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Database.PG.Query       as Q
import qualified Text.Shakespeare.Text   as ST


data OpVar = OLD | NEW deriving (Show)

pgIdenTrigger:: Ops -> TriggerName -> T.Text
pgIdenTrigger op trn = pgFmtIden . qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> T.pack (show op')

getDropFuncSql :: Ops -> TriggerName -> T.Text
getDropFuncSql op trn = "DROP FUNCTION IF EXISTS"
                        <> " hdb_views." <> pgIdenTrigger op trn <> "()"
                        <> " CASCADE"

mkAllTriggersQ
  :: (MonadTx m, HasSQLGenCtx m)
  => TriggerName
  -> QualifiedTable
  -> [PGColumnInfo]
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
  -> [PGColumnInfo]
  -> Ops
  -> SubscribeOpSpec
  -> m ()
mkTriggerQ trn qt allCols op (SubscribeOpSpec columns payload) = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  liftTx $ Q.multiQE defaultTxErrorHandler $ Q.fromText . TL.toStrict $
    let payloadColumns = fromMaybe SubCStar payload
        mkQId opVar colInfo = toJSONableExp strfyNum (pgiType colInfo) False $
          S.SEQIden $ S.QIden (opToQual opVar) $ toIden $ pgiColumn colInfo
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

        operation = T.pack $ show op
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
    opToTxt = T.pack . show

delTriggerQ :: TriggerName -> Q.TxE QErr ()
delTriggerQ trn = mapM_ (\op -> Q.unitQE
                          defaultTxErrorHandler
                          (Q.fromText $ getDropFuncSql op trn) () False) [INSERT, UPDATE, DELETE]

addEventTriggerToCatalog
  :: QualifiedTable
  -> EventTriggerConf
  -> Q.TxE QErr ()
addEventTriggerToCatalog qt etc = do
  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.event_triggers
                       (name, type, schema_name, table_name, configuration, comment)
           VALUES ($1, 'table', $2, $3, $4)
         |] (name, sn, tn, Q.AltJ $ toJSON etc) False
  where
    QualifiedObject sn tn = qt
    (EventTriggerConf name _ _ _ _ _ _) = etc

delEventTriggerFromCatalog :: TriggerName -> Q.TxE QErr ()
delEventTriggerFromCatalog trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.event_triggers
           WHERE name = $1
                |] (Identity trn) False
  delTriggerQ trn
  archiveEvents trn

archiveEvents :: TriggerName -> Q.TxE QErr ()
archiveEvents trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |] (Identity trn) False

fetchEvent :: EventId -> Q.TxE QErr (EventId, Bool)
fetchEvent eid = do
  events <- Q.listQE defaultTxErrorHandler
            [Q.sql|
              SELECT l.id, l.locked
              FROM hdb_catalog.event_log l
              JOIN hdb_catalog.event_triggers e
              ON l.trigger_name = e.name
              WHERE l.id = $1
              |] (Identity eid) True
  event <- getEvent events
  assertEventUnlocked event
  return event
  where
    getEvent []    = throw400 NotExists "event not found"
    getEvent (x:_) = return x

    assertEventUnlocked (_, locked) = when locked $
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

subTableP1 :: (UserInfoM m, QErrM m, CacheRM m) => CreateEventTriggerQuery -> m (QualifiedTable, Bool, EventTriggerConf)
subTableP1 (CreateEventTriggerQuery name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace comment) = do
  ti <- askTableCoreInfo qt
  -- can only replace for same table
  when replace $ do
    ti' <- _tiCoreInfo <$> askTabInfoFromTrigger name
    when (_tciName ti' /= _tciName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (qt, replace, EventTriggerConf name (TriggerOpsDef insert update delete enableManual) webhook webhookFromEnv rconf mheaders comment)
  where
    assertCols _ Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols (assertPGCol (_tciFieldInfoMap ti) "")

subTableP2Setup
  :: (QErrM m, MonadIO m)
  => QualifiedTable -> EventTriggerConf -> m (EventTriggerInfo, [SchemaDependency])
subTableP2Setup qt (EventTriggerConf name def webhook webhookFromEnv rconf mheaders comment) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing)    -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _                    -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf webhookConf
  headerInfos <- getHeaderInfosFromConf headerConfs
  let eTrigInfo = EventTriggerInfo name def rconf webhookInfo headerInfos
      tabDep = SchemaDependency (SOTable qt) DRParent
  pure (eTrigInfo, tabDep:getTrigDefDeps qt def)

getTrigDefDeps :: QualifiedTable -> TriggerOpsDef -> [SchemaDependency]
getTrigDefDeps qt (TriggerOpsDef mIns mUpd mDel _) =
  mconcat $ catMaybes [ subsOpSpecDeps <$> mIns
                      , subsOpSpecDeps <$> mUpd
                      , subsOpSpecDeps <$> mDel
                      ]
  where
    subsOpSpecDeps :: SubscribeOpSpec -> [SchemaDependency]
    subsOpSpecDeps os =
      let cols = getColsFromSub $ sosColumns os
          colDeps = flip map cols $ \col ->
            SchemaDependency (SOTableObj qt (TOCol col)) DRColumn
          payload = maybe [] getColsFromSub (sosPayload os)
          payloadDeps = flip map payload $ \col ->
            SchemaDependency (SOTableObj qt (TOCol col)) DRPayload
        in colDeps <> payloadDeps
    getColsFromSub sc = case sc of
      SubCStar         -> []
      SubCArray pgcols -> pgcols

subTableP2
  :: (MonadTx m)
  => QualifiedTable -> Bool -> EventTriggerConf -> m ()
subTableP2 qt replace etc = liftTx if replace
  then updateEventTriggerInCatalog etc
  else addEventTriggerToCatalog qt etc

runCreateEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => CreateEventTriggerQuery -> m EncJSON
runCreateEventTriggerQuery q = do
  (qt, replace, etc) <- subTableP1 q
  subTableP2 qt replace etc
  buildSchemaCacheFor $ MOTableObj qt (MTOTrigger $ etcName etc)
  return successMsg

runDeleteEventTriggerQuery
  :: (MonadTx m, CacheRWM m)
  => DeleteEventTriggerQuery -> m EncJSON
runDeleteEventTriggerQuery (DeleteEventTriggerQuery name) = do
  liftTx $ delEventTriggerFromCatalog name
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

deliverEvent
  :: (QErrM m, MonadTx m)
  => RedeliverEventQuery -> m EncJSON
deliverEvent (RedeliverEventQuery eventId) = do
  _ <- liftTx $ fetchEvent eventId
  liftTx $ markForDelivery eventId
  return successMsg

runRedeliverEvent
  :: (MonadTx m)
  => RedeliverEventQuery -> m EncJSON
runRedeliverEvent = deliverEvent

insertManualEvent
  :: QualifiedTable
  -> TriggerName
  -> Value
  -> Q.TxE QErr EventId
insertManualEvent qt trn rowData = do
  let op = T.pack $ show MANUAL
  eids <- map runIdentity <$> Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
                |] (sn, tn, trn, op, Q.AltJ $ toJSON rowData) True
  getEid eids
  where
    QualifiedObject sn tn = qt
    getEid []    = throw500 "could not create manual event"
    getEid (x:_) = return x

runInvokeEventTrigger
  :: (QErrM m, CacheRM m, MonadTx m)
  => InvokeEventTriggerQuery -> m EncJSON
runInvokeEventTrigger (InvokeEventTriggerQuery name payload) = do
  trigInfo <- askEventTriggerInfo name
  assertManual $ etiOpsDef trigInfo
  ti  <- askTabInfoFromTrigger name
  eid <- liftTx $ insertManualEvent (_tciName $ _tiCoreInfo ti) name payload
  return $ encJFromJValue $ object ["event_id" .= eid]
  where
    assertManual (TriggerOpsDef _ _ _ man) = case man of
      Just True -> return ()
      _         -> throw400 NotSupported "manual mode is not enabled for event trigger"

getHeaderInfosFromConf
  :: (QErrM m, MonadIO m)
  => [HeaderConf] -> m [EventHeaderInfo]
getHeaderInfosFromConf = mapM getHeader
  where
    getHeader :: (QErrM m, MonadIO m) => HeaderConf -> m EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> return $ EventHeaderInfo hconf val
      (HeaderConf _ (HVEnv val))   -> do
        envVal <- getEnv val
        return $ EventHeaderInfo hconf envVal

getWebhookInfoFromConf
  :: (QErrM m, MonadIO m) => WebhookConf -> m WebhookConfInfo
getWebhookInfoFromConf wc = case wc of
  WCValue w -> return $ WebhookConfInfo wc w
  WCEnv we -> do
    envVal <- getEnv we
    return $ WebhookConfInfo wc envVal

getEnv :: (QErrM m, MonadIO m) => T.Text -> m T.Text
getEnv env = do
  mEnv <- liftIO $ lookupEnv (T.unpack env)
  case mEnv of
    Nothing     -> throw400 NotFound $ "environment variable '" <> env <> "' not set"
    Just envVal -> return (T.pack envVal)

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

updateEventTriggerInCatalog :: EventTriggerConf -> Q.TxE QErr ()
updateEventTriggerInCatalog trigConf =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      UPDATE hdb_catalog.event_triggers
      SET
      configuration = $1
      WHERE name = $2
    |] (Q.AltJ $ toJSON trigConf, etcName trigConf) True
