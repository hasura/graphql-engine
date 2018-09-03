{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Hasura.RQL.DDL.Subscribe where

import           Data.Aeson
import qualified Data.FileEmbed      as FE
import qualified Data.HashMap.Strict as HashMap
import           Data.Int            (Int64)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Database.PG.Query   as Q
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import qualified Text.Ginger         as TG

data Ops = INSERT | UPDATE | DELETE deriving (Show)

data OpVar = OLD | NEW deriving (Show)

type GingerTmplt = TG.Template TG.SourcePos

defaultNumRetries :: Int64
defaultNumRetries = 0

defaultRetryInterval :: Int64
defaultRetryInterval = 10

parseGingerTmplt :: TG.Source -> Either String GingerTmplt
parseGingerTmplt src = either parseE Right res
  where
    res = runIdentity $ TG.parseGinger' parserOptions src
    parserOptions = TG.mkParserOptions resolver
    resolver = const $ return Nothing
    parseE e = Left $ TG.formatParserError (Just "") e

triggerTmplt :: Maybe GingerTmplt
triggerTmplt = case parseGingerTmplt $(FE.embedStringFile "src-rsr/trigger.sql") of
  Left e      -> Nothing
  Right tmplt -> Just tmplt

getDropFuncSql :: Ops -> TriggerName -> T.Text
getDropFuncSql op trn = "DROP FUNCTION IF EXISTS"
                        <> " hdb_views.notify_skor_" <> trn <> "_" <> T.pack (show op) <> "()"
                        <> " CASCADE"

getTriggerSql :: Ops -> TriggerName -> SchemaName -> TableName -> Maybe SubscribeOpSpec -> Maybe T.Text
getTriggerSql op name sn tn spec =
  let globalCtx =  HashMap.fromList [
                    (T.pack "NAME", name)
                  , (T.pack "SCHEMA_NAME", getSchemaTxt sn)
                  , (T.pack "TABLE_NAME", getTableTxt tn)]
      opCtx = maybe HashMap.empty (createOpCtx op) spec
      context = HashMap.union globalCtx opCtx
  in
      spec >> renderSql context <$> triggerTmplt
  where
    createOpCtx :: Ops -> SubscribeOpSpec -> HashMap.HashMap T.Text T.Text
    createOpCtx op1 (SubscribeOpSpec columns) = HashMap.fromList [
                                        (T.pack "OPERATION", T.pack $ show op1)
                                      , (T.pack "OLD_DATA_EXPRESSION", renderOldDataExp op1 columns )
                                      , (T.pack "NEW_DATA_EXPRESSION", renderNewDataExp op1 columns )]
    renderOldDataExp :: Ops -> SubscribeColumns -> T.Text
    renderOldDataExp op2 scs = case op2 of
                                 INSERT -> "NULL"
                                 UPDATE -> getRowExpression OLD scs
                                 DELETE -> getRowExpression OLD scs
    renderNewDataExp :: Ops -> SubscribeColumns -> T.Text
    renderNewDataExp op2 scs = case op2 of
                                 INSERT -> getRowExpression NEW scs
                                 UPDATE -> getRowExpression NEW scs
                                 DELETE -> "NULL"
    getRowExpression :: OpVar -> SubscribeColumns -> T.Text
    getRowExpression opVar scs = case scs of
                                    SubCStar -> "row_to_json(" <> T.pack (show opVar) <> ")"
                                    SubCArray cols -> "row_to_json((select r from (select " <> listcols cols opVar <> ") as r))"
                                   where
                                     listcols :: [PGCol] -> OpVar -> T.Text
                                     listcols pgcols var = T.intercalate ", " $ fmap (mkQualified (T.pack $ show var).getPGColTxt) pgcols
                                     mkQualified :: T.Text -> T.Text -> T.Text
                                     mkQualified v col = v <> "." <> col

    renderSql :: HashMap.HashMap T.Text T.Text -> GingerTmplt -> T.Text
    renderSql = TG.easyRender

mkTriggerQ
  :: TriggerName
  -> QualifiedTable
  -> TriggerOpsDef
  -> Q.TxE QErr ()
mkTriggerQ name (QualifiedTable sn tn) (TriggerOpsDef insert update delete) = do
  let msql = getTriggerSql INSERT name sn tn insert
             <> getTriggerSql UPDATE name sn tn update
             <> getTriggerSql DELETE name sn tn delete
  case msql of
    Just sql -> Q.multiQE defaultTxErrorHandler (Q.fromBuilder $ TE.encodeUtf8Builder sql)
    Nothing -> throw500 "no trigger sql generated"

addEventTriggerToCatalog :: QualifiedTable -> EventTriggerDef
               -> Q.TxE QErr ()
addEventTriggerToCatalog (QualifiedTable sn tn) (EventTriggerDef name def webhook rconf) = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers (name, type, schema_name, table_name, definition, webhook)
                                  VALUES ($1, 'table', $2, $3, $4, $5)
                                  |] (name, sn, tn, Q.AltJ $ toJSON def, webhook) True

  mkTriggerQ name (QualifiedTable sn tn) def

  let rconfTup = (rcNumRetries rconf, rcIntervalSec rconf)

  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers_retry_conf (name, num_retries, interval_seconds)
                                  VALUES ($1, $2, $3)
                                  |] (name, fst rconfTup, snd rconfTup) True

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
    tx op = Q.multiQE defaultTxErrorHandler (Q.fromBuilder $ TE.encodeUtf8Builder $ getDropFuncSql op trn)

fetchEventTrigger :: TriggerName -> Q.TxE QErr EventTrigger
fetchEventTrigger trn = do
  triggers <- Q.listQE defaultTxErrorHandler [Q.sql|
                                              SELECT e.schema_name, e.table_name, e.name, e.definition::json, e.webhook, r.num_retries, r.interval_seconds
                                              FROM hdb_catalog.event_triggers e
                                              JOIN hdb_catalog.event_triggers_retry_conf r
                                              ON e.name = r.name
                                              WHERE e.name = $1
                                  |] (Identity trn) True
  getTrigger triggers
  where
    getTrigger []    = throw404 "Could not fetch event trigger"
    getTrigger (x:_) = return $ EventTrigger (QualifiedTable sn tn) trn tDef webhook (toRetryConf nr rint)
      where (sn, tn, trn, Q.AltJ tDef, webhook, nr, rint) = x

subTableP1 :: (P1C m) => SubscribeTableQuery -> m (QualifiedTable, EventTriggerDef)
subTableP1 (SubscribeTableQuery name qt insert update delete retryConf webhook) = do
  ti <- askTabInfo qt
  assertCols ti insert
  assertCols ti update
  assertCols ti delete
  let rconf = fromMaybe (RetryConf defaultNumRetries defaultRetryInterval) retryConf
  return (qt, EventTriggerDef name (TriggerOpsDef insert update delete) webhook rconf)
  where
    assertCols ti Nothing = return ()
    assertCols ti (Just sos) = do
      let cols = sosColumns sos
      case cols of
        SubCStar         -> return ()
        SubCArray pgcols -> forM_ pgcols columnExists
                            where
                              listcols =  map pgiName $ getCols $ tiFieldInfoMap ti
                              columnExists col = if col `elem` listcols
                                                 then return ()
                                                 else throw400  NotExists $ "column '" <> getPGColTxt col <> "' does not exist"

subTableP2 :: (P2C m) => QualifiedTable -> EventTriggerDef -> m ()
subTableP2 qt q@(EventTriggerDef name def webhook rconf) = do
  liftTx $ addEventTriggerToCatalog qt q
  addEventTriggerToCache qt name def rconf webhook

subTableP2shim :: (P2C m) => (QualifiedTable, EventTriggerDef) -> m RespBody
subTableP2shim (qt, etdef) = do
  subTableP2 qt etdef
  return successMsg

instance HDBQuery SubscribeTableQuery where
  type Phase1Res SubscribeTableQuery = (QualifiedTable, EventTriggerDef)
  phaseOne = subTableP1
  phaseTwo _ q = subTableP2shim q
  schemaCachePolicy = SCPReload

unsubTableP1 :: (P1C m) => UnsubscribeTableQuery -> m ()
unsubTableP1 _ = return ()

unsubTableP2 :: (P2C m) => UnsubscribeTableQuery -> m RespBody
unsubTableP2 (UnsubscribeTableQuery name) = do
  et <- liftTx $ fetchEventTrigger name
  delEventTriggerFromCache (etTable et) name
  liftTx $ delEventTriggerFromCatalog name
  return successMsg

instance HDBQuery UnsubscribeTableQuery where
  type Phase1Res UnsubscribeTableQuery = ()
  phaseOne = unsubTableP1
  phaseTwo q _ = unsubTableP2 q
  schemaCachePolicy = SCPReload
