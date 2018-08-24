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
                        <> " hdb_views.notify_skor_" <> trn <> "_" <> T.pack (show op)
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
                                      , (T.pack "DATA_EXPRESSION", renderDataExp op1 columns )]
    renderDataExp :: Ops -> SubscribeColumns -> T.Text
    renderDataExp op2 scs = let recVar = case op2 of
                                 DELETE -> "OLD"
                                 _      -> "NEW"
                            in case scs of
                                 SubCStar -> "row_to_json(" <> recVar <> ")"
                                 SubCArray cols -> "row_to_json((select r from (select " <> listcols cols recVar <> ") as r))"
                                   where
                                     listcols :: [PGCol] -> T.Text -> T.Text
                                     listcols pgcols var = T.intercalate ", " $ fmap (mkQualified var.getPGColTxt) pgcols
                                     mkQualified :: T.Text -> T.Text -> T.Text
                                     mkQualified v col = v <> "." <> col

    renderSql :: HashMap.HashMap T.Text T.Text -> GingerTmplt -> T.Text
    renderSql = TG.easyRender

subscribeTable :: SubscribeTableQuery
               -> Q.TxE QErr RespBody
subscribeTable (SubscribeTableQuery name (QualifiedTable sn tn) insert update delete retryConf webhook) = do
  let def = TriggerDefinition insert update delete
  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers (name, type, schema_name, table_name, definition, webhook)
                                  VALUES ($1, 'table', $2, $3, $4, $5)
                                  |] (name, sn, tn, Q.AltJ $ toJSON def, webhook) True
  let triggerSQL = getTriggerSql INSERT name sn tn insert
                   <> getTriggerSql UPDATE name sn tn update
                   <> getTriggerSql DELETE name sn tn delete
  tx triggerSQL

  liftIO $ print retryConf

  let rConf = case retryConf of
        Just conf -> (fromMaybe defaultNumRetries $ rcNumRetries conf,  fromMaybe defaultRetryInterval $ rcIntervalSec conf)
        Nothing   -> (defaultNumRetries, defaultRetryInterval)

  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers_retry_conf (name, num_retries, interval_seconds)
                                  VALUES ($1, $2, $3)
                                  |] (name, fst rConf, snd rConf) True
  return successMsg
  where
    tx:: Maybe T.Text -> Q.TxE QErr ()
    tx (Just sql) = Q.multiQE defaultTxErrorHandler (Q.fromBuilder $ TE.encodeUtf8Builder sql)
    tx Nothing = throw500 "no trigger sql generated"

unsubscribeTable :: UnsubscribeTableQuery
               -> Q.TxE QErr RespBody
unsubscribeTable (UnsubscribeTableQuery name) = do
  delEventTriggerFromCatalog name
  return successMsg

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

subTableP1 :: (P1C m) => SubscribeTableQuery -> m ()
subTableP1 _ = return ()

subTableP2 :: (P2C m) => SubscribeTableQuery -> m RespBody
subTableP2 q = liftTx $ subscribeTable q

instance HDBQuery SubscribeTableQuery where
  type Phase1Res SubscribeTableQuery = ()
  phaseOne = subTableP1
  phaseTwo q _ = subTableP2 q
  schemaCachePolicy = SCPNoChange

unsubTableP1 :: (P1C m) => UnsubscribeTableQuery -> m ()
unsubTableP1 _ = return ()

unsubTableP2 :: (P2C m) => UnsubscribeTableQuery -> m RespBody
unsubTableP2 q = liftTx $ unsubscribeTable q

instance HDBQuery UnsubscribeTableQuery where
  type Phase1Res UnsubscribeTableQuery = ()
  phaseOne = unsubTableP1
  phaseTwo q _ = unsubTableP2 q
  schemaCachePolicy = SCPNoChange
