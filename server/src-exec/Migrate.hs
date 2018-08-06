{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Migrate
  ( migrateCatalog
  , curCatalogVer
  ) where

import qualified Data.ByteString.Builder     as BB
import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q

import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission   (dropView)
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Time.Clock             (UTCTime)

curCatalogVer :: T.Text
curCatalogVer = "1.1"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

hdbViewsSchema :: SchemaName
hdbViewsSchema = SchemaName "hdb_views"

getCatalogVersion :: Q.TxE QErr T.Text
getCatalogVersion = do
  res <- Q.withQE defaultTxErrorHandler [Q.sql|
                SELECT version FROM hdb_catalog.hdb_version
                    |] () False
  return $ runIdentity $ Q.getRow res

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

updateCatalogWithNewViews :: SchemaCache -> Q.TxE QErr ()
updateCatalogWithNewViews sc = processCatalogChanges sc schemaDiff
  where
    schemaDiff = SchemaDiff [] alteredViews
    alteredViews = flip map allViews $ \v ->
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
  updateCatalogWithNewViews preMigrateSchema
  where
    permTable = TableName "hdb_permission"
    relTable = TableName "hdb_relationship"

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
