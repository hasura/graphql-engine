module Hasura.RQL.DDL.Subscribe
  ( CreateEventTriggerQuery
  , runCreateEventTriggerQuery
  , DeleteEventTriggerQuery
  , runDeleteEventTriggerQuery
  , DeliverEventQuery
  , runDeliverEvent

  -- TODO: review
  , delEventTriggerFromCatalog
  , subTableP2
  , subTableP2Setup
  , mkTriggerQ
  ) where

import           Data.Aeson
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types
import           System.Environment      (lookupEnv)

import qualified Hasura.SQL.DML          as S

import qualified Data.FileEmbed          as FE
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Text               as T
import qualified Database.PG.Query       as Q


data OpVar = OLD | NEW deriving (Show)

triggerTmplt :: Maybe GingerTmplt
triggerTmplt = case parseGingerTmplt $(FE.embedStringFile "src-rsr/trigger.sql.j2") of
  Left _      -> Nothing
  Right tmplt -> Just tmplt

pgIdenTrigger:: Ops -> TriggerName -> T.Text
pgIdenTrigger op trn = pgFmtIden (qualifyTriggerName op trn)
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> T.pack (show op')

getDropFuncSql :: Ops -> TriggerName -> T.Text
getDropFuncSql op trn = "DROP FUNCTION IF EXISTS"
                        <> " hdb_views." <> pgIdenTrigger op trn <> "()"
                        <> " CASCADE"

getTriggerSql
  :: Ops
  -> TriggerId
  -> TriggerName
  -> QualifiedTable
  -> [PGColInfo]
  -> Maybe SubscribeOpSpec
  -> Maybe T.Text
getTriggerSql op trid trn qt allCols spec =
  let globalCtx =  HashMap.fromList
                   [ (T.pack "ID", trid)
                   , (T.pack "NAME", trn)
                   , (T.pack "QUALIFIED_TRIGGER_NAME", pgIdenTrigger op trn)
                   , (T.pack "QUALIFIED_TABLE", toSQLTxt qt)
                   ]
      opCtx = maybe HashMap.empty (createOpCtx op) spec
      context = HashMap.union globalCtx opCtx
  in
      spec >> renderGingerTmplt context <$> triggerTmplt
  where
    createOpCtx op1 (SubscribeOpSpec columns payload) =
      HashMap.fromList
      [ (T.pack "OPERATION", T.pack $ show op1)
      , (T.pack "OLD_ROW", toSQLTxt $ renderRow OLD columns )
      , (T.pack "NEW_ROW", toSQLTxt $ renderRow NEW columns )
      , (T.pack "OLD_PAYLOAD_EXPRESSION", toSQLTxt $ renderOldDataExp op1 $ fromMaybePayload payload )
      , (T.pack "NEW_PAYLOAD_EXPRESSION", toSQLTxt $ renderNewDataExp op1 $ fromMaybePayload payload )
      ]
    renderOldDataExp op2 scs =
      case op2 of
        INSERT -> S.SEUnsafe "NULL"
        UPDATE -> getRowExpression OLD scs
        DELETE -> getRowExpression OLD scs
    renderNewDataExp op2 scs =
      case op2 of
        INSERT -> getRowExpression NEW scs
        UPDATE -> getRowExpression NEW scs
        DELETE -> S.SEUnsafe "NULL"
    getRowExpression opVar scs =
      case scs of
        SubCStar -> applyRowToJson $ S.SEUnsafe $ opToTxt opVar
        SubCArray cols -> applyRowToJson $
          S.mkRowExp $ map (toExtr . mkQId opVar) $
          getColInfos cols allCols

    applyRowToJson e = S.SEFnApp "row_to_json" [e] Nothing
    applyRow e = S.SEFnApp "row" [e] Nothing
    toExtr = flip S.Extractor Nothing
    mkQId opVar colInfo = toJSONableExp (pgiType colInfo) $
      S.SEQIden $ S.QIden (opToQual opVar) $ toIden $ pgiName colInfo

    opToQual = S.QualVar . opToTxt
    opToTxt = T.pack . show

    renderRow opVar scs =
      case scs of
        SubCStar -> applyRow $ S.SEUnsafe $ opToTxt opVar
        SubCArray cols -> applyRow $
          S.mkRowExp $ map (toExtr . mkQId opVar) $
          getColInfos cols allCols

    fromMaybePayload = fromMaybe SubCStar

mkTriggerQ
  :: TriggerId
  -> TriggerName
  -> QualifiedTable
  -> [PGColInfo]
  -> TriggerOpsDef
  -> Q.TxE QErr ()
mkTriggerQ trid trn qt allCols (TriggerOpsDef insert update delete) = do
  let msql = getTriggerSql INSERT trid trn qt allCols insert
             <> getTriggerSql UPDATE trid trn qt allCols update
             <> getTriggerSql DELETE trid trn qt allCols delete
  case msql of
    Just sql -> Q.multiQE defaultTxErrorHandler (Q.fromText sql)
    Nothing  -> throw500 "no trigger sql generated"

delTriggerQ :: TriggerName -> Q.TxE QErr ()
delTriggerQ trn = mapM_ (\op -> Q.unitQE
                          defaultTxErrorHandler
                          (Q.fromText $ getDropFuncSql op trn) () False) [INSERT, UPDATE, DELETE]

addEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColInfo]
  -> EventTriggerConf
  -> Q.TxE QErr TriggerId
addEventTriggerToCatalog qt allCols etc = do
  ids <- map runIdentity <$> Q.listQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.event_triggers
                       (name, type, schema_name, table_name, configuration)
           VALUES ($1, 'table', $2, $3, $4)
           RETURNING id
               |] (name, sn, tn, Q.AltJ $ toJSON etc) True

  trid <- getTrid ids
  mkTriggerQ trid name qt allCols opsdef
  return trid
  where
    QualifiedObject sn tn = qt
    (EventTriggerConf name opsdef _ _ _ _) = etc
    getTrid []    = throw500 "could not create event-trigger"
    getTrid (x:_) = return x

delEventTriggerFromCatalog :: TriggerName -> Q.TxE QErr ()
delEventTriggerFromCatalog trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.event_triggers
           WHERE name = $1
                |] (Identity trn) True
  delTriggerQ trn

updateEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColInfo]
  -> EventTriggerConf
  -> Q.TxE QErr TriggerId
updateEventTriggerToCatalog qt allCols etc = do
  ids <- map runIdentity <$> Q.listQE defaultTxErrorHandler
         [Q.sql|
           UPDATE hdb_catalog.event_triggers
           SET
           configuration = $1
           WHERE name = $2
           RETURNING id
               |] (Q.AltJ $ toJSON etc, name) True
  trid <- getTrid ids
  delTriggerQ name
  mkTriggerQ trid name qt allCols opsdef
  return trid
  where
    EventTriggerConf name opsdef _ _ _ _ = etc
    getTrid []    = throw500 "could not update event-trigger"
    getTrid (x:_) = return x

fetchEvent :: EventId -> Q.TxE QErr (EventId, Bool)
fetchEvent eid = do
  events <- Q.listQE defaultTxErrorHandler
            [Q.sql|
              SELECT l.id, l.locked
              FROM hdb_catalog.event_log l
              JOIN hdb_catalog.event_triggers e
              ON l.trigger_id = e.id
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
subTableP1 (CreateEventTriggerQuery name qt insert update delete retryConf webhook webhookFromEnv mheaders replace) = do
  adminOnly
  ti <- askTabInfo qt
  -- can only replace for same table
  when replace $ do
    ti' <- askTabInfoFromTrigger name
    when (tiName ti' /= tiName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe defaultRetryConf retryConf
  return (qt, replace, EventTriggerConf name (TriggerOpsDef insert update delete) webhook webhookFromEnv rconf mheaders)
  where
    assertCols _ Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols (assertPGCol (tiFieldInfoMap ti) "")

--(QErrM m, CacheRWM m, MonadTx m, MonadIO m)

subTableP2Setup
  :: (QErrM m, CacheRWM m, MonadIO m)
  => QualifiedTable -> TriggerId -> EventTriggerConf -> m ()
subTableP2Setup qt trid (EventTriggerConf name def webhook webhookFromEnv rconf mheaders) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing)    -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _                    -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf webhookConf
  headerInfos <- getHeaderInfosFromConf headerConfs
  let eTrigInfo = EventTriggerInfo trid name def rconf webhookInfo headerInfos
      tabDep = SchemaDependency (SOTable qt) "parent"
  addEventTriggerToCache qt eTrigInfo (tabDep:getTrigDefDeps qt def)

getTrigDefDeps :: QualifiedTable -> TriggerOpsDef -> [SchemaDependency]
getTrigDefDeps qt (TriggerOpsDef mIns mUpd mDel) =
  mconcat $ catMaybes [ subsOpSpecDeps <$> mIns
                      , subsOpSpecDeps <$> mUpd
                      , subsOpSpecDeps <$> mDel
                      ]
  where
    subsOpSpecDeps :: SubscribeOpSpec -> [SchemaDependency]
    subsOpSpecDeps os =
      let cols = getColsFromSub $ sosColumns os
          colDeps = flip map cols $ \col ->
            SchemaDependency (SOTableObj qt (TOCol col)) "column"
          payload = maybe [] getColsFromSub (sosPayload os)
          payloadDeps = flip map payload $ \col ->
            SchemaDependency (SOTableObj qt (TOCol col)) "payload"
        in colDeps <> payloadDeps
    getColsFromSub sc = case sc of
      SubCStar         -> []
      SubCArray pgcols -> pgcols

subTableP2
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m)
  => QualifiedTable -> Bool -> EventTriggerConf -> m ()
subTableP2 qt replace etc = do
  allCols <- getCols . tiFieldInfoMap <$> askTabInfo qt
  trid <- if replace
    then do
    delEventTriggerFromCache qt (etcName etc)
    liftTx $ updateEventTriggerToCatalog qt allCols etc
    else
    liftTx $ addEventTriggerToCatalog qt allCols etc
  subTableP2Setup qt trid etc

runCreateEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m)
  => CreateEventTriggerQuery -> m RespBody
runCreateEventTriggerQuery q = do
  (qt, replace, etc) <- subTableP1 q
  subTableP2 qt replace etc
  return successMsg

unsubTableP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => DeleteEventTriggerQuery -> m QualifiedTable
unsubTableP1 (DeleteEventTriggerQuery name)  = do
  adminOnly
  ti <- askTabInfoFromTrigger name
  return $ tiName ti

unsubTableP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DeleteEventTriggerQuery -> QualifiedTable -> m RespBody
unsubTableP2 (DeleteEventTriggerQuery name) qt = do
  delEventTriggerFromCache qt name
  liftTx $ delEventTriggerFromCatalog name
  return successMsg

runDeleteEventTriggerQuery
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DeleteEventTriggerQuery -> m RespBody
runDeleteEventTriggerQuery q =
  unsubTableP1 q >>= unsubTableP2 q

deliverEvent
  :: (QErrM m, MonadTx m)
  => DeliverEventQuery -> m RespBody
deliverEvent (DeliverEventQuery eventId) = do
  _ <- liftTx $ fetchEvent eventId
  liftTx $ markForDelivery eventId
  return successMsg

runDeliverEvent
  :: (QErrM m, UserInfoM m, MonadTx m)
  => DeliverEventQuery -> m RespBody
runDeliverEvent q =
  adminOnly >> deliverEvent q

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
