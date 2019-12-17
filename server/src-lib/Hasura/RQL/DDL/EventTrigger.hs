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
  , getEventTriggerDef
  , updateEventTriggerDef
  ) where

import           Data.Aeson
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           System.Environment      (lookupEnv)

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
  :: TriggerName
  -> QualifiedTable
  -> [PGColumnInfo]
  -> Bool
  -> TriggerOpsDef
  -> Q.TxE QErr ()
mkAllTriggersQ trn qt allCols strfyNum fullspec = do
  let insertDef = tdInsert fullspec
      updateDef = tdUpdate fullspec
      deleteDef = tdDelete fullspec
  onJust insertDef (mkTriggerQ trn qt allCols strfyNum INSERT)
  onJust updateDef (mkTriggerQ trn qt allCols strfyNum UPDATE)
  onJust deleteDef (mkTriggerQ trn qt allCols strfyNum DELETE)

mkTriggerQ
  :: TriggerName
  -> QualifiedTable
  -> [PGColumnInfo]
  -> Bool
  -> Ops
  -> SubscribeOpSpec
  -> Q.TxE QErr ()
mkTriggerQ trn qt allCols strfyNum op (SubscribeOpSpec columns payload) =
  Q.multiQE defaultTxErrorHandler $ Q.fromText . TL.toStrict $
    let name = triggerNameToTxt trn
        qualifiedTriggerName = pgIdenTrigger op trn
        qualifiedTable = toSQLTxt qt

        operation = T.pack $ show op
        oldRow = toSQLTxt $ renderRow OLD columns
        newRow = toSQLTxt $ renderRow NEW columns
        oldPayloadExpression = toSQLTxt . renderOldDataExp op $ fromMaybePayload payload
        newPayloadExpression = toSQLTxt . renderNewDataExp op $ fromMaybePayload payload
    in $(ST.stextFile "src-rsr/trigger.sql.shakespeare")
  where
    renderOldDataExp op2 scs =
      case op2 of
        INSERT -> S.SENull
        UPDATE -> getRowExpression OLD scs
        DELETE -> getRowExpression OLD scs
        MANUAL -> S.SENull
    renderNewDataExp op2 scs =
      case op2 of
        INSERT -> getRowExpression NEW scs
        UPDATE -> getRowExpression NEW scs
        DELETE -> S.SENull
        MANUAL -> S.SENull
    getRowExpression opVar scs =
      case scs of
        SubCStar -> applyRowToJson $ S.SEUnsafe $ opToTxt opVar
        SubCArray cols -> applyRowToJson $
          S.mkRowExp $ map (toExtr . mkQId opVar) $
          getColInfos cols allCols

    applyRowToJson e = S.SEFnApp "row_to_json" [e] Nothing
    applyRow e = S.SEFnApp "row" [e] Nothing
    toExtr = flip S.Extractor Nothing
    mkQId opVar colInfo = toJSONableExp strfyNum (pgiType colInfo) False $
      S.SEQIden $ S.QIden (opToQual opVar) $ toIden $ pgiColumn colInfo

    opToQual = S.QualVar . opToTxt
    opToTxt = T.pack . show

    renderRow opVar scs =
      case scs of
        SubCStar -> applyRow $ S.SEUnsafe $ opToTxt opVar
        SubCArray cols -> applyRow $
          S.mkRowExp $ map (toExtr . mkQId opVar) $
          getColInfos cols allCols

    fromMaybePayload = fromMaybe SubCStar

delTriggerQ :: TriggerName -> Q.TxE QErr ()
delTriggerQ trn = mapM_ (\op -> Q.unitQE
                          defaultTxErrorHandler
                          (Q.fromText $ getDropFuncSql op trn) () False) [INSERT, UPDATE, DELETE]

addEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColumnInfo]
  -> Bool
  -> EventTriggerConf
  -> Q.TxE QErr ()
addEventTriggerToCatalog qt allCols strfyNum etc = do
  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.event_triggers
                       (name, type, schema_name, table_name, configuration)
           VALUES ($1, 'table', $2, $3, $4)
         |] (name, sn, tn, Q.AltJ $ toJSON etc) False

  mkAllTriggersQ name qt allCols strfyNum fullspec
  where
    QualifiedObject sn tn = qt
    (EventTriggerConf name fullspec _ _ _ _) = etc

delEventTriggerFromCatalog :: TriggerName -> Q.TxE QErr ()
delEventTriggerFromCatalog trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.event_triggers
           WHERE name = $1
                |] (Identity trn) False
  delTriggerQ trn
  archiveEvents trn

archiveEvents:: TriggerName -> Q.TxE QErr ()
archiveEvents trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |] (Identity trn) False

updateEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColumnInfo]
  -> Bool
  -> EventTriggerConf
  -> Q.TxE QErr ()
updateEventTriggerToCatalog qt allCols strfyNum etc = do
  updateEventTriggerDef name etc
  delTriggerQ name
  mkAllTriggersQ name qt allCols strfyNum fullspec
 where
    EventTriggerConf name fullspec _ _ _ _ = etc

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
subTableP1 (CreateEventTriggerQuery name qt insert update delete enableManual retryConf webhook webhookFromEnv mheaders replace) = do
  ti <- askTabInfo qt
  -- can only replace for same table
  when replace $ do
    ti' <- askTabInfoFromTrigger name
    when (_tiName ti' /= _tiName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (qt, replace, EventTriggerConf name (TriggerOpsDef insert update delete enableManual) webhook webhookFromEnv rconf mheaders)
  where
    assertCols _ Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols (assertPGCol (_tiFieldInfoMap ti) "")

--(QErrM m, CacheRWM m, MonadTx m, MonadIO m)

subTableP2Setup
  :: (QErrM m, CacheRWM m, MonadIO m)
  => QualifiedTable -> EventTriggerConf -> m ()
subTableP2Setup qt (EventTriggerConf name def webhook webhookFromEnv rconf mheaders) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing)    -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _                    -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf webhookConf
  headerInfos <- getHeaderInfosFromConf headerConfs
  let eTrigInfo = EventTriggerInfo name def rconf webhookInfo headerInfos
      tabDep = SchemaDependency (SOTable qt) DRParent
  addEventTriggerToCache qt eTrigInfo (tabDep:getTrigDefDeps qt def)

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
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasSQLGenCtx m)
  => QualifiedTable -> Bool -> EventTriggerConf -> m ()
subTableP2 qt replace etc = do
  allCols <- getCols . _tiFieldInfoMap <$> askTabInfo qt
  strfyNum <- stringifyNum <$> askSQLGenCtx
  if replace
    then do
    delEventTriggerFromCache qt (etcName etc)
    liftTx $ updateEventTriggerToCatalog qt allCols strfyNum etc
    else
    liftTx $ addEventTriggerToCatalog qt allCols strfyNum etc
  subTableP2Setup qt etc

runCreateEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasSQLGenCtx m)
  => CreateEventTriggerQuery -> m EncJSON
runCreateEventTriggerQuery q = do
  (qt, replace, etc) <- subTableP1 q
  subTableP2 qt replace etc
  return successMsg

unsubTableP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => DeleteEventTriggerQuery -> m QualifiedTable
unsubTableP1 (DeleteEventTriggerQuery name)  = do
  ti <- askTabInfoFromTrigger name
  return $ _tiName ti

unsubTableP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DeleteEventTriggerQuery -> QualifiedTable -> m EncJSON
unsubTableP2 (DeleteEventTriggerQuery name) qt = do
  delEventTriggerFromCache qt name
  liftTx $ delEventTriggerFromCatalog name
  return successMsg

runDeleteEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DeleteEventTriggerQuery -> m EncJSON
runDeleteEventTriggerQuery q =
  unsubTableP1 q >>= unsubTableP2 q

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
  eid <-liftTx $ insertManualEvent (_tiName ti) name payload
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
    Nothing -> throw400 NotFound $ "environment variable '" <> env <> "' not set"
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

updateEventTriggerDef
  :: TriggerName -> EventTriggerConf -> Q.TxE QErr ()
updateEventTriggerDef trigName trigConf =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      UPDATE hdb_catalog.event_triggers
      SET
      configuration = $1
      WHERE name = $2
    |] (Q.AltJ $ toJSON trigConf, trigName) True
