-- | Functions for fetching and updating @'Metadata' in the catalog.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchMetadataTx
  , setMetadataTx
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                   as Q

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata

fetchMetadataTx :: Q.TxE QErr Metadata
fetchMetadataTx = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata where id = 1
    |] () True
  case rows of
    []                             -> pure emptyMetadata
    [(Identity (Q.AltJ metadata))] -> pure metadata
    _                              -> throw500 "multiple rows in hdb_metadata table"

setMetadataTx :: Metadata -> Q.TxE QErr ()
setMetadataTx metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_metadata
       (id, metadata) VALUES (1, $1::json)
     ON CONFLICT (id) DO UPDATE SET metadata = $1::json
    |] (Identity $ Q.AltJ metadata) True
