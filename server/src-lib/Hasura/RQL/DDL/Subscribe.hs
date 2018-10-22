{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Hasura.RQL.DDL.Subscribe where

import           Data.Aeson
import           Data.Int                (Int64)
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types
import           System.Environment      (lookupEnv)

import qualified Hasura.SQL.DML          as S

import qualified Data.FileEmbed          as FE
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Database.PG.Query       as Q

data Ops = INSERT | UPDATE | DELETE deriving (Show)

data OpVar = OLD | NEW deriving (Show)

defaultNumRetries :: Int
defaultNumRetries = 0

defaultRetryInterval :: Int
defaultRetryInterval = 10

triggerTmplt :: Maybe GingerTmplt
triggerTmplt = case parseGingerTmplt $(FE.embedStringFile "src-rsr/trigger.sql.j2") of
  Left _      -> Nothing
  Right tmplt -> Just tmplt

getDropFuncSql :: Ops -> TriggerName -> T.Text
getDropFuncSql op trn = "DROP FUNCTION IF EXISTS"
                        <> " hdb_views.notify_hasura_" <> trn <> "_" <> T.pack (show op) <> "()"
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
                   , (T.pack "QUALIFIED_TABLE", toSQLTxt qt)
                   ]
      opCtx = maybe HashMap.empty (createOpCtx op) spec
      context = HashMap.union globalCtx opCtx
  in
      spec >> renderGingerTmplt context <$> triggerTmplt
  where
    createOpCtx op1 (SubscribeOpSpec columns) =
      HashMap.fromList
      [ (T.pack "OPERATION", T.pack $ show op1)
      , (T.pack "OLD_DATA_EXPRESSION", toSQLTxt $ renderOldDataExp op1 columns )
      , (T.pack "NEW_DATA_EXPRESSION", toSQLTxt $ renderNewDataExp op1 columns )
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
    toExtr = flip S.Extractor Nothing
    mkQId opVar colInfo = toJSONableExp (pgiType colInfo) $
      S.SEQIden $ S.QIden (opToQual opVar) $ toIden $ pgiName colInfo

    opToQual = S.QualVar . opToTxt
    opToTxt = T.pack . show


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
    Nothing -> throw500 "no trigger sql generated"

addEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColInfo]
  -> EventTriggerDef
  -> Q.TxE QErr TriggerId
addEventTriggerToCatalog qt@(QualifiedTable sn tn) allCols (EventTriggerDef name def webhook rconf mheaders) = do
  ids <- map runIdentity <$> Q.listQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers (name, type, schema_name, table_name, definition, webhook, num_retries, retry_interval, headers)
                                  VALUES ($1, 'table', $2, $3, $4, $5, $6, $7, $8)
                                  RETURNING id
                                  |] (name, sn, tn, Q.AltJ $ toJSON def, webhook, toInt64 $ rcNumRetries rconf, toInt64 $ rcIntervalSec rconf, Q.AltJ $ toJSON mheaders) True

  trid <- getTrid ids
  mkTriggerQ trid name qt allCols def
  return trid
  where
    getTrid []    = throw500 "could not create event-trigger"
    getTrid (x:_) = return x

delEventTriggerFromCatalog :: TriggerName -> Q.TxE QErr ()
delEventTriggerFromCatalog trn = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.event_triggers
           WHERE name = $1
                |] (Identity trn) True
  mapM_ tx [INSERT, UPDATE, DELETE]
  where
    tx :: Ops -> Q.TxE QErr ()
    tx op = Q.multiQE defaultTxErrorHandler (Q.fromText $ getDropFuncSql op trn)

updateEventTriggerToCatalog
  :: QualifiedTable
  -> [PGColInfo]
  -> EventTriggerDef
  -> Q.TxE QErr TriggerId
updateEventTriggerToCatalog qt allCols (EventTriggerDef name def webhook rconf mheaders) = do
  ids <- map runIdentity <$> Q.listQE defaultTxErrorHandler [Q.sql|
                                  UPDATE hdb_catalog.event_triggers
                                  SET
                                  definition = $1,
                                  webhook = $2,
                                  num_retries = $3,
                                  retry_interval = $4,
                                  headers = $5
                                  WHERE name = $6
                                  RETURNING id
                                  |] (Q.AltJ $ toJSON def, webhook, toInt64 $ rcNumRetries rconf, toInt64 $ rcIntervalSec rconf, Q.AltJ $ toJSON mheaders, name) True
  trid <- getTrid ids
  mkTriggerQ trid name qt allCols def
  return trid
  where
    getTrid []    = throw500 "could not update event-trigger"
    getTrid (x:_) = return x


fetchEventTrigger :: TriggerName -> Q.TxE QErr EventTrigger
fetchEventTrigger trn = do
  triggers <- Q.listQE defaultTxErrorHandler [Q.sql|
                                              SELECT e.schema_name, e.table_name, e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval
                                              FROM hdb_catalog.event_triggers e
                                              WHERE e.name = $1
                                  |] (Identity trn) True
  getTrigger triggers
  where
    getTrigger []    = throw400 NotExists ("could not find event trigger '" <> trn <> "'")
    getTrigger (x:_) = return $ EventTrigger (QualifiedTable sn tn) trn' tDef webhook (RetryConf nr rint)
      where (sn, tn, trn', Q.AltJ tDef, webhook, nr, rint) = x

fetchEvent :: EventId -> Q.TxE QErr (EventId, Bool)
fetchEvent eid = do
  events <- Q.listQE defaultTxErrorHandler [Q.sql|
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

subTableP1 :: (P1C m) => CreateEventTriggerQuery -> m (QualifiedTable, Bool, EventTriggerDef)
subTableP1 (CreateEventTriggerQuery name qt insert update delete retryConf webhook mheaders replace) = do
  adminOnly
  ti <- askTabInfo qt
  -- can only replace for same table
  when replace $ do
    ti' <- askTabInfoFromTrigger name
    when (tiName ti' /= tiName ti) $ throw400 NotSupported "cannot replace table or schema for trigger"

  assertCols ti insert
  assertCols ti update
  assertCols ti delete

  let rconf = fromMaybe (RetryConf defaultNumRetries defaultRetryInterval) retryConf
  return (qt, replace, EventTriggerDef name (TriggerOpsDef insert update delete) webhook rconf mheaders)
  where
    assertCols _ Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols (assertPGCol (tiFieldInfoMap ti) "")

subTableP2 :: (P2C m) => QualifiedTable -> Bool -> EventTriggerDef -> m ()
subTableP2 qt replace q@(EventTriggerDef name def webhook rconf mheaders) = do
  allCols <- (getCols . tiFieldInfoMap) <$> askTabInfo qt
  trid <- if replace
    then do
    delEventTriggerFromCache qt name
    liftTx $ updateEventTriggerToCatalog qt allCols q
    else
    liftTx $ addEventTriggerToCatalog qt allCols q
  let headerConfs = fromMaybe [] mheaders
  headers <- getHeadersFromConf headerConfs
  addEventTriggerToCache qt trid name def rconf webhook headers

subTableP2shim :: (P2C m) => (QualifiedTable, Bool, EventTriggerDef) -> m RespBody
subTableP2shim (qt, replace, etdef) = do
  subTableP2 qt replace etdef
  return successMsg

instance HDBQuery CreateEventTriggerQuery where
  type Phase1Res CreateEventTriggerQuery = (QualifiedTable, Bool, EventTriggerDef)
  phaseOne = subTableP1
  phaseTwo _ = subTableP2shim
  schemaCachePolicy = SCPReload

unsubTableP1 :: (P1C m) => DeleteEventTriggerQuery -> m QualifiedTable
unsubTableP1 (DeleteEventTriggerQuery name)  = do
  adminOnly
  ti <- askTabInfoFromTrigger name
  return $ tiName ti

unsubTableP2 :: (P2C m) => QualifiedTable -> DeleteEventTriggerQuery -> m RespBody
unsubTableP2 qt (DeleteEventTriggerQuery name) = do
  delEventTriggerFromCache qt name
  liftTx $ delEventTriggerFromCatalog name
  return successMsg

instance HDBQuery DeleteEventTriggerQuery where
  type Phase1Res DeleteEventTriggerQuery = QualifiedTable
  phaseOne = unsubTableP1
  phaseTwo q qt = unsubTableP2 qt q
  schemaCachePolicy = SCPReload

deliverEvent :: (P2C m) => DeliverEventQuery -> m RespBody
deliverEvent (DeliverEventQuery eventId) = do
  _ <- liftTx $ fetchEvent eventId
  liftTx $ markForDelivery eventId
  return successMsg

instance HDBQuery DeliverEventQuery where
  type Phase1Res DeliverEventQuery = ()
  phaseOne _ = adminOnly
  phaseTwo q _ = deliverEvent q
  schemaCachePolicy = SCPNoChange

getHeadersFromConf :: (P2C m) => [HeaderConf] -> m [(HeaderName, T.Text)]
getHeadersFromConf = mapM getHeader
  where
    getHeader :: (P2C m) => HeaderConf -> m (HeaderName, T.Text)
    getHeader hconf = case hconf of
      (HeaderConf name (HVValue val)) -> return (name, val)
      (HeaderConf name (HVEnv val))   -> do
        mEnv <- liftIO $ lookupEnv (T.unpack val)
        case mEnv of
          Nothing -> throw400 NotFound $ "environment variable '" <> val <> "' not set"
          Just val' -> return (name, T.pack val')

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral
