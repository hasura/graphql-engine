-- | Functions for loading and modifying the catalog. See the module documentation for
-- "Hasura.RQL.DDL.Schema" for more details.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchCatalogData
  , saveTableToCatalog
  , updateTableIsEnumInCatalog
  , updateTableConfig
  , deleteTableFromCatalog
  , getTableConfig
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query            as Q

import           Data.Aeson

import           Hasura.Db
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Types

fetchCatalogData :: (MonadTx m) => m CatalogMetadata
fetchCatalogData = liftTx $ Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  $(Q.sqlFromFile "src-rsr/catalog_metadata.sql") () True

saveTableToCatalog :: (MonadTx m) => QualifiedTable -> SystemDefined -> Bool -> TableConfig -> m ()
saveTableToCatalog (QualifiedObject sn tn) systemDefined isEnum config = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO "hdb_catalog"."hdb_table"
      (table_schema, table_name, is_system_defined, is_enum, configuration)
    VALUES ($1, $2, $3, $4, $5)
  |] (sn, tn, systemDefined, isEnum, configVal) False
  where
    configVal = Q.AltJ $ toJSON config

updateTableIsEnumInCatalog :: (MonadTx m) => QualifiedTable -> Bool -> m ()
updateTableIsEnumInCatalog (QualifiedObject sn tn) isEnum = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
      UPDATE "hdb_catalog"."hdb_table" SET is_enum = $3
      WHERE table_schema = $1 AND table_name = $2
    |] (sn, tn, isEnum) False

updateTableConfig :: (MonadTx m) => QualifiedTable -> TableConfig -> m ()
updateTableConfig (QualifiedObject sn tn) config = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE "hdb_catalog"."hdb_table"
              SET configuration = $1
            WHERE table_schema = $2 AND table_name = $3
                |] (configVal, sn, tn) False
  where
    configVal = Q.AltJ $ toJSON config

deleteTableFromCatalog :: (MonadTx m) => QualifiedTable -> m ()
deleteTableFromCatalog (QualifiedObject sn tn) = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM "hdb_catalog"."hdb_table"
    WHERE table_schema = $1 AND table_name = $2
  |] (sn, tn) False

getTableConfig :: MonadTx m => QualifiedTable -> m TableConfig
getTableConfig (QualifiedObject sn tn) = liftTx $
  Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT configuration::json FROM hdb_catalog.hdb_table
        WHERE table_schema = $1 AND table_name = $2
    |] (sn, tn) True
