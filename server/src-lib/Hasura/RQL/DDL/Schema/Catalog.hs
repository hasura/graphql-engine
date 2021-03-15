-- | Functions for fetching and updating @'Metadata' in the catalog.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchMetadataFromCatalog
  , fetchMetadataAndResourceVersionFromCatalog
  , insertMetadataInCatalog
  , setMetadataInCatalog
  , bumpMetadataVersionInCatalog
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                   as Q

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata

import           Data.Int                            (Int64)

fetchMetadataFromCatalog :: Q.TxE QErr Metadata
fetchMetadataFromCatalog = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata
    |] () True
  case rows of
    []                           -> pure emptyMetadata
    [Identity (Q.AltJ metadata)] -> pure metadata
    _                            -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataAndResourceVersionFromCatalog :: Q.TxE QErr (Metadata, MetadataResourceVersion)
fetchMetadataAndResourceVersionFromCatalog = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata, resource_version from hdb_catalog.hdb_metadata
    |] () True
  case rows of
    []                                   -> pure (emptyMetadata, initialResourceVersion)
    [(Q.AltJ metadata, resourceVersion)] -> pure (metadata, MetadataResourceVersion resourceVersion)
    _                                    -> throw500 "multiple rows in hdb_metadata table"

-- Used to increment metadata version when no other changes are required
bumpMetadataVersionInCatalog :: Q.TxE QErr  ()
bumpMetadataVersionInCatalog = do
    Q.unitQE defaultTxErrorHandler
      [Q.sql|
      UPDATE hdb_catalog.hdb_metadata
      SET resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      |] () True

insertMetadataInCatalog :: Metadata -> Q.TxE QErr ()
insertMetadataInCatalog metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
    INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
    VALUES (1, $1::json)
    |] (Identity $ Q.AltJ metadata) True

setMetadataInCatalog :: Maybe MetadataResourceVersion -> Metadata -> Q.TxE QErr ()
setMetadataInCatalog mResourceVersion metadata = case mResourceVersion of
  -- If a resource version isn't specified update the metadata and bump the version
  Nothing ->
    Q.unitQE defaultTxErrorHandler
      [Q.sql|
      INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
      VALUES (1, $1::json)
      ON CONFLICT (id) DO UPDATE SET
        metadata = $1::json,
        resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      |] (Identity $ Q.AltJ metadata) True

  -- If a resource version is specified, check that it matches and...
  --   If so: Update the metadata and bump the version
  --   If not: Throw a 409 error
  Just resourceVersion -> do
    rows <- Q.withQE defaultTxErrorHandler
      [Q.sql|
      INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
      VALUES (1, $1::json)
      ON CONFLICT (id) DO UPDATE SET
        metadata = $1::json,
        resource_version = hdb_catalog.hdb_metadata.resource_version + 1
        WHERE hdb_catalog.hdb_metadata.resource_version = $2
      RETURNING 1
      |] (Q.AltJ metadata, getMetadataResourceVersion resourceVersion) True
    case rows of
      []                    -> throw409 $ "metadata resource version referenced (" <>  tshow (getMetadataResourceVersion resourceVersion) <> ") did not match current version"
      [Identity (_::Int64)] -> pure ()
      _                     -> throw500 "multiple rows in hdb_metadata table"

