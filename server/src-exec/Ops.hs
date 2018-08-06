{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ops
  ( initCatalogSafe
  , cleanCatalog
  , migrateCatalog
  , execQuery
  ) where

import           TH

import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission   (dropView)
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.Server.Query
import           Hasura.SQL.Types

import qualified Database.PG.Query           as Q

import           Data.Time.Clock             (UTCTime)

import qualified Data.Aeson                  as A
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T

curCatalogVer :: T.Text
curCatalogVer = "1.1"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

hdbViewsSchema :: SchemaName
hdbViewsSchema = SchemaName "hdb_views"

initCatalogSafe :: UTCTime -> Q.TxE QErr String
initCatalogSafe initTime =  do
  hdbCatalogExists <- Q.catchE defaultTxErrorHandler $
                      doesSchemaExist $ SchemaName "hdb_catalog"
  bool (initCatalogStrict True initTime) onCatalogExists hdbCatalogExists
  where
    onCatalogExists = do
      versionExists <- Q.catchE defaultTxErrorHandler $
                       doesVersionTblExist
                       (SchemaName "hdb_catalog") (TableName "hdb_version")
      bool (initCatalogStrict False initTime) (return initialisedMsg) versionExists

    initialisedMsg = "initialise: the state is already initialised"

    doesVersionTblExist sn tblN =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_tables
                WHERE schemaname = $1 AND tablename = $2)
               |] (sn, tblN) False

    doesSchemaExist sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM information_schema.schemata
                WHERE schema_name = $1
           )
                    |] (Identity sn) False

initCatalogStrict :: Bool -> UTCTime -> Q.TxE QErr String
initCatalogStrict createSchema initTime =  do
  Q.catchE defaultTxErrorHandler $ do

    when createSchema $ do
      Q.unitQ "CREATE SCHEMA hdb_catalog" () False
      -- This is where the generated views and triggers are stored
      Q.unitQ "CREATE SCHEMA hdb_views" () False

    flExtExists <- isExtInstalled "first_last_agg"

    if flExtExists then Q.unitQ "CREATE EXTENSION first_last_agg SCHEMA hdb_catalog" () False
    else Q.multiQ $(Q.sqlFromFile "src-rsr/first_last.sql") >>= \(Q.Discard _) -> return ()

    Q.Discard () <- Q.multiQ $(Q.sqlFromFile "src-rsr/init_catalog.sql")
    initDefaultViews

  -- Build the metadata query
  tx <- liftEither $ buildTxAny adminUserInfo emptySchemaCache metadataQuery

  -- Execute the query
  void $ snd <$> tx
  setAsSystemDefined >> addVersion initTime
  return "initialise: successfully initialised"
  where
    addVersion modTime = Q.catchE defaultTxErrorHandler $
      Q.unitQ [Q.sql|
                INSERT INTO "hdb_catalog"."hdb_version" VALUES ($1, $2)
                |] (curCatalogVer, modTime) False

    setAsSystemDefined = Q.catchE defaultTxErrorHandler $ do
      Q.unitQ "UPDATE hdb_catalog.hdb_table SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_relationship SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_permission SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_query_template SET is_system_defined = 'true'" () False

    isExtInstalled :: T.Text -> Q.Tx Bool
    isExtInstalled sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_catalog.pg_available_extensions
                WHERE name = $1
           )
                    |] (Identity sn) False


cleanCatalog :: Q.TxE QErr ()
cleanCatalog = Q.catchE defaultTxErrorHandler $ do
  -- This is where the generated views and triggers are stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
  Q.unitQ "DROP SCHEMA hdb_catalog CASCADE" () False

getCatalogVersion :: Q.TxE QErr T.Text
getCatalogVersion = do
  res <- Q.withQE defaultTxErrorHandler [Q.sql|
                SELECT version FROM hdb_catalog.hdb_version
                    |] () False
  return $ runIdentity $ Q.getRow res

dropFKeyConstraints :: QualifiedTable -> Q.TxE QErr ()
dropFKeyConstraints (QualifiedTable sn tn) = do
  constraints <- map runIdentity <$>
    Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT constraint_name
           FROM information_schema.table_constraints
           WHERE table_schema = $1
             AND table_name = $2
             AND constraint_type = 'FOREIGN KEY'
          |] (sn, tn) False
  forM_ constraints $ \constraint ->
    Q.unitQE defaultTxErrorHandler (dropConstQ constraint) () False
  where
    dropConstQ cName = Q.fromBuilder $ BB.string7 $ T.unpack $
                       "ALTER TABLE " <> getSchemaTxt sn <> "."
                       <> getTableTxt tn
                       <> " DROP CONSTRAINT " <> cName

addFKeyOnUpdateCascade :: QualifiedTable -> Q.TxE QErr ()
addFKeyOnUpdateCascade (QualifiedTable sn tn) =
  Q.unitQE defaultTxErrorHandler addFKeyQ () False
  where
    addFKeyQ = Q.fromBuilder $ BB.string7 $ T.unpack $
      "ALTER TABLE " <> getSchemaTxt sn <> "."
      <> getTableTxt tn <> " ADD FOREIGN KEY (table_schema, table_name)"
      <> " REFERENCES hdb_catalog.hdb_table(table_schema, table_name)"
      <> " ON UPDATE CASCADE"

permAggView :: TableName
permAggView = TableName "hdb_permission_agg"

fkeyView :: TableName
fkeyView = TableName "hdb_foreign_key_constraint"

checkView :: TableName
checkView = TableName "hdb_check_constraint"

uniqueView :: TableName
uniqueView = TableName "hdb_unique_constraint"

pkeyView :: TableName
pkeyView = TableName "hdb_primary_key"

allViews :: [TableName]
allViews = [permAggView, fkeyView, checkView, uniqueView, pkeyView]

dropViewsInHdbCatalog :: Q.Tx ()
dropViewsInHdbCatalog =
  mapM_ (dropView . QualifiedTable hdbCatalogSchema) allViews

applyCatalogChanges :: SchemaCache -> Q.TxE QErr ()
applyCatalogChanges sc = processCatalogChanges sc schemaDiff
  where
    schemaDiff = SchemaDiff [] alterTabs
    alterTabs = flip map allViews $ \v ->
      let qt = QualifiedTable hdbCatalogSchema v
          tdiff = TableDiff (Just $ QualifiedTable hdbViewsSchema v)
                            [] [] [] []
      in (qt, tdiff)


migrateFrom10 :: Q.TxE QErr ()
migrateFrom10 = do
  forM_ [permTable, relTable] $ \t -> do
    dropFKeyConstraints $ QualifiedTable hdbCatalogSchema t
    addFKeyOnUpdateCascade $ QualifiedTable hdbCatalogSchema t

  Q.catchE defaultTxErrorHandler initDefaultViews
  preMigrateSchema <- buildSchemaCache
  Q.catchE defaultTxErrorHandler dropViewsInHdbCatalog
  applyCatalogChanges preMigrateSchema
  where
    permTable = TableName "hdb_permission"
    relTable = TableName "hdb_relationship"

migrateFrom08 :: Q.TxE QErr ()
migrateFrom08 =
  -- From 0.8 to 1
  Q.catchE defaultTxErrorHandler $ do
    Q.unitQ "ALTER TABLE hdb_catalog.hdb_relationship ADD COLUMN comment TEXT NULL" () False
    Q.unitQ "ALTER TABLE hdb_catalog.hdb_permission ADD COLUMN comment TEXT NULL" () False
    Q.unitQ "ALTER TABLE hdb_catalog.hdb_query_template ADD COLUMN comment TEXT NULL" () False
    Q.unitQ [Q.sql|
          UPDATE hdb_catalog.hdb_query_template
             SET template_defn =
                 json_build_object('type', 'select', 'args', template_defn->'select');
                |] () False

migrateCatalog :: UTCTime -> Q.TxE QErr String
migrateCatalog migrationTime = do
  preVer <- getCatalogVersion
  if | preVer == curCatalogVer ->
       return "migrate: already at the latest version"
     | preVer == "0.8" -> do
         migrateFrom08
         migrateFrom10
         afterMigrate
     | preVer == "1" -> do
         migrateFrom10
         afterMigrate
     | otherwise -> throw400 NotSupported $
                    "migrate: unsupported version : " <> preVer
  where
    afterMigrate = do
      -- update the catalog version
      updateVersion
      -- clean hdb_views
      Q.unitQE defaultTxErrorHandler "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
      Q.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_views" () False
      Q.catchE defaultTxErrorHandler initDefaultViews

      -- try building the schema cache
      void buildSchemaCache
      return "migrate: successfully migrated"

    updateVersion =
      Q.unitQE defaultTxErrorHandler [Q.sql|
                UPDATE "hdb_catalog"."hdb_version"
                   SET "version" = $1,
                       "upgraded_on" = $2
                    |] (curCatalogVer, migrationTime) False

execQuery :: BL.ByteString -> Q.TxE QErr BL.ByteString
execQuery queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  schemaCache <- buildSchemaCache
  tx <- liftEither $ buildTxAny adminUserInfo schemaCache query
  fst <$> tx
