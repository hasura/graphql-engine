-- | Functions for loading and modifying the catalog. See the module documentation for
-- "Hasura.RQL.DDL.Schema" for more details.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchCatalogData
  , saveTableToCatalog
  , updateTableIsEnumInCatalog
  , deleteTableFromCatalog
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query        as Q

import           Hasura.Db
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

fetchCatalogData :: (MonadTx m) => m CatalogMetadata
fetchCatalogData = liftTx $ Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  $(Q.sqlFromFile "src-rsr/catalog_metadata.sql") () True

saveTableToCatalog :: (MonadTx m) => QualifiedTable -> Bool -> m ()
saveTableToCatalog (QualifiedObject sn tn) isEnum = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO "hdb_catalog"."hdb_table" (table_schema, table_name, is_enum)
    VALUES ($1, $2, $3)
  |] (sn, tn, isEnum) False

updateTableIsEnumInCatalog :: (MonadTx m) => QualifiedTable -> Bool -> m ()
updateTableIsEnumInCatalog (QualifiedObject sn tn) isEnum =
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
      UPDATE "hdb_catalog"."hdb_table" SET is_enum = $3
      WHERE table_schema = $1 AND table_name = $2
    |] (sn, tn, isEnum) False

deleteTableFromCatalog :: (MonadTx m) => QualifiedTable -> m ()
deleteTableFromCatalog (QualifiedObject sn tn) = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM "hdb_catalog"."hdb_table"
    WHERE table_schema = $1 AND table_name = $2
  |] (sn, tn) False
