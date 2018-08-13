{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Hasura.RQL.DDL.SubscribeTable where

import qualified Database.PG.Query   as Q

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Text.Ginger

data Ops = INSERT | UPDATE | DELETE | ALL

getTriggerSql:: Ops -> SchemaName -> TableName -> Maybe SubscribeOpSpec -> Maybe T.Text
getTriggerSql op sn tn spec =
  let globalCtx =  HashMap.fromList [
                    (T.pack "SCHEMA_NAME", getSchemaTxt sn)
                  , (T.pack "TABLE_NAME", getTableTxt tn)]
  in
  case op of
  INSERT->
    let opCtx = maybe HashMap.empty (\(SubscribeOpSpec columns) ->
                                       HashMap.fromList [
                                        (T.pack "OPERATION", "INSERT")
                                      , (T.pack "DATA_EXPRESSION", renderDataExp INSERT columns )]
                                    ) spec
        context = HashMap.union globalCtx opCtx
    in
      spec >> (Just $ renderSql context)
  UPDATE->
    let opCtx = maybe HashMap.empty (\(SubscribeOpSpec columns) ->
                                       HashMap.fromList [
                                        (T.pack "OPERATION", "UPDATE")
                                      , (T.pack "DATA_EXPRESSION", renderDataExp UPDATE columns )]
                                    ) spec
        context = HashMap.union globalCtx opCtx
    in
      spec >> (Just $ renderSql context)
  DELETE->
    let opCtx = maybe HashMap.empty (\(SubscribeOpSpec columns) ->
                                       HashMap.fromList [
                                        (T.pack "OPERATION", "DELETE")
                                      , (T.pack "DATA_EXPRESSION", renderDataExp DELETE columns )]
                                    ) spec
        context = HashMap.union globalCtx opCtx
    in
      spec >> (Just $ renderSql context)
  ALL -> getTriggerSql INSERT sn tn spec <> getTriggerSql UPDATE sn tn spec <> getTriggerSql DELETE sn tn spec
  where
    renderDataExp :: Ops -> SubscribeColumns -> T.Text
    renderDataExp op' scs = let recVar = case op' of
                                 DELETE -> "OLD"
                                 _      -> "NEW"
                            in case scs of
                                 SubCStar -> T.pack $ "row_to_json(" ++ recVar ++ ")"
                                 SubCArray cols -> T.pack $ "row_to_json((select r from (select "++ listcols cols ++ ") as r))"
                                   where
                                     listcols :: [PGCol] -> String
                                     listcols pgcols = L.intercalate ", " $ fmap (\c -> T.unpack $ getPGColTxt c) pgcols
    renderSql :: HashMap.HashMap T.Text T.Text -> T.Text
    renderSql context = let triggerTemplate = "CREATE OR REPLACE function hdb_catalog.notify_skor_{{SCHEMA_NAME}}_{{TABLE_NAME}}_{{OPERATION}}() RETURNS trigger \
                                  \LANGUAGE plpgsql \
                                  \AS $$ \
                                  \DECLARE \
                                  \payload json; \
                                  \BEGIN \
                                  \raise notice 'entered trigger'; \
                                  \payload := json_build_object( \
                                  \                     'table', TG_TABLE_NAME, \
                                  \                     'schema', TG_TABLE_SCHEMA, \
                                  \                     'op', TG_OP, \
                                  \                     'data', {{DATA_EXPRESSION}} \
                                  \                     )::text; \
                                  \raise notice '%', payload; \
                                  \INSERT INTO \
                                  \hdb_catalog.event_source_log (payload, webhook) \
                                  \SELECT payload, e.webhook \
                                  \FROM hdb_catalog.event_sources e \
                                  \WHERE e.schema_name='{{SCHEMA_NAME}}' AND e.table_name='{{TABLE_NAME}}'; \
                                  \raise notice 'finished trigger'; \
                                  \RETURN NULL; \
                                  \END; \
                                  \$$; \
                                  \DROP TRIGGER IF EXISTS notify_skor_{{SCHEMA_NAME}}_{{TABLE_NAME}}_{{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}}; \
                                  \CREATE TRIGGER notify_skor_{{SCHEMA_NAME}}_{{TABLE_NAME}}_{{OPERATION}} AFTER {{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}} FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.notify_skor_{{SCHEMA_NAME}}_{{TABLE_NAME}}_{{OPERATION}}();"

                            template = either (error . show) id . runIdentity $ parseGinger nullResolver Nothing triggerTemplate
                        in easyRender context template
                           where
                             nullResolver :: IncludeResolver Identity
                             nullResolver = const $ return Nothing

subscribeTable :: SubscribeTableQuery
               -> Q.TxE QErr RespBody
subscribeTable (SubscribeTableQuery (QualifiedTable sn tn) insert update delete columns webhook) = do

  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_sources (type, schema_name, table_name, webhook)
                                  VALUES ('table', $1, $2, $3)
                                  |] (sn, tn, webhook) True
  let triggerSQL = getTriggerSql INSERT sn tn insert
                   <> getTriggerSql UPDATE sn tn update
                   <> getTriggerSql DELETE sn tn delete
                   <> getTriggerSql ALL sn tn (SubscribeOpSpec <$> columns)

  tx triggerSQL
  return successMsg
  where
    tx:: Maybe T.Text -> Q.TxE QErr ()
    tx (Just sql) = Q.multiQE defaultTxErrorHandler (Q.fromBuilder $ TE.encodeUtf8Builder sql)
    tx Nothing = throw500 "no trigger sql generated"

subTableP1 :: (P1C m) => SubscribeTableQuery -> m ()
subTableP1 _ = return ()

subTableP2 :: (P2C m) => SubscribeTableQuery -> m RespBody
subTableP2 q = liftTx $ subscribeTable q

instance HDBQuery SubscribeTableQuery where
  type Phase1Res SubscribeTableQuery = ()
  phaseOne = subTableP1
  phaseTwo q _ = subTableP2 q
  schemaCachePolicy = SCPNoChange
