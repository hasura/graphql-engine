{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Hasura.RQL.DDL.SubscribeTable where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Database.PG.Query   as Q
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Text.Ginger

data Ops = INSERT | UPDATE | DELETE deriving (Show)

type TriggerName = T.Text

data TriggerDefinition
  = TriggerDefinition
  { tdInsert :: !(Maybe SubscribeOpSpec)
  , tdUpdate :: !(Maybe SubscribeOpSpec)
  , tdDelete :: !(Maybe SubscribeOpSpec)
   }
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerDefinition)

getTriggerSql:: Ops -> TriggerName -> SchemaName -> TableName -> Maybe SubscribeOpSpec -> Maybe T.Text
getTriggerSql op name sn tn spec =
  let globalCtx =  HashMap.fromList [
                    (T.pack "NAME", name)
                  , (T.pack "SCHEMA_NAME", getSchemaTxt sn)
                  , (T.pack "TABLE_NAME", getTableTxt tn)]
      opCtx = maybe HashMap.empty (createOpCtx op) spec
      context = HashMap.union globalCtx opCtx
  in
      spec >> (Just $ renderSql context)
  where
    createOpCtx :: Ops -> SubscribeOpSpec -> HashMap.HashMap T.Text T.Text
    createOpCtx op1 (SubscribeOpSpec columns) = HashMap.fromList [
                                        (T.pack "OPERATION", T.pack $ show op1)
                                      , (T.pack "DATA_EXPRESSION", renderDataExp INSERT columns )]
    renderDataExp :: Ops -> SubscribeColumns -> T.Text
    renderDataExp op2 scs = let recVar = case op2 of
                                 DELETE -> "OLD"
                                 _      -> "NEW"
                            in case scs of
                                 SubCStar -> T.pack $ "row_to_json(" ++ recVar ++ ")"
                                 SubCArray cols -> T.pack $ "row_to_json((select r from (select "++ listcols cols ++ ") as r))"
                                   where
                                     listcols :: [PGCol] -> String
                                     listcols pgcols = L.intercalate ", " $ fmap (T.unpack.getPGColTxt) pgcols
    renderSql :: HashMap.HashMap T.Text T.Text -> T.Text
    renderSql context = let triggerTemplate = "CREATE OR REPLACE function hdb_catalog.notify_skor_{{NAME}}_{{OPERATION}}() RETURNS trigger \
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
                                  \hdb_catalog.events_log (payload, webhook) \
                                  \SELECT payload, e.webhook \
                                  \FROM hdb_catalog.event_triggers e \
                                  \WHERE e.name='{{NAME}}'; \
                                  \raise notice 'finished trigger'; \
                                  \RETURN NULL; \
                                  \END; \
                                  \$$; \
                                  \DROP TRIGGER IF EXISTS notify_skor_{{NAME}}_{{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}}; \
                                  \CREATE TRIGGER notify_skor_{{NAME}}_{{OPERATION}} AFTER {{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}} FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.notify_skor_{{NAME}}_{{OPERATION}}();"

                            template = either (error . show) id . runIdentity $ parseGinger nullResolver Nothing triggerTemplate
                        in easyRender context template
                           where
                             nullResolver :: IncludeResolver Identity
                             nullResolver = const $ return Nothing

subscribeTable :: SubscribeTableQuery
               -> Q.TxE QErr RespBody
subscribeTable (SubscribeTableQuery name (QualifiedTable sn tn) insert update delete webhook) = do
  let def = TriggerDefinition insert update delete
  Q.unitQE defaultTxErrorHandler [Q.sql|
                                  INSERT into hdb_catalog.event_triggers (name, type, schema_name, table_name, definition, webhook)
                                  VALUES ($1, 'table', $2, $3, $4, $5)
                                  |] (name, sn, tn, Q.AltJ $ toJSON def, webhook) True
  let triggerSQL = getTriggerSql INSERT name sn tn insert
                   <> getTriggerSql UPDATE name sn tn update
                   <> getTriggerSql DELETE name sn tn delete

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
