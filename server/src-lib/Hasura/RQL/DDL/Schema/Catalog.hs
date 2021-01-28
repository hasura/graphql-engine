-- | Functions for fetching and updating @'Metadata' in the catalog.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchMetadataFromCatalog
  , setMetadataInCatalog
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                   as Q

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata

fetchMetadataFromCatalog :: Q.TxE QErr Metadata
fetchMetadataFromCatalog = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata where id = 1
    |] () True
  case rows of
    []                             -> pure emptyMetadata
    [(Identity (Q.AltJ metadata))] -> pure metadata
    _                              -> throw500 "multiple rows in hdb_metadata table"

setMetadataInCatalog :: Metadata -> Q.TxE QErr ()
setMetadataInCatalog metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_metadata
       (id, metadata) VALUES (1, $1::json)
     ON CONFLICT (id) DO UPDATE SET metadata = $1::json
    |] (Identity $ Q.AltJ metadata) True
