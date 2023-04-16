{-# LANGUAGE QuasiQuotes #-}

-- | Functions for fetching and updating @'Metadata' in the catalog.
module Hasura.RQL.DDL.Schema.Catalog
  ( fetchMetadataFromCatalog,
    fetchMetadataAndResourceVersionFromCatalog,
    fetchMetadataResourceVersionFromCatalog,
    fetchMetadataNotificationsFromCatalog,
    insertMetadataInCatalog,
    setMetadataInCatalog,
    bumpMetadataVersionInCatalog,
  )
where

import Data.Bifunctor (bimap)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
  ( MetadataResourceVersion (..),
    MetadataWithResourceVersion (..),
    initialResourceVersion,
  )
import Hasura.RQL.Types.SchemaCache.Build (CacheInvalidations)
import Hasura.Server.Types (InstanceId (..))

fetchMetadataFromCatalog :: PG.TxE QErr Metadata
fetchMetadataFromCatalog = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
       SELECT metadata from hdb_catalog.hdb_metadata
    |]
      ()
      True
  case rows of
    [] -> pure emptyMetadata
    [Identity (PG.ViaJSON metadata)] -> pure metadata
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataAndResourceVersionFromCatalog :: PG.TxE QErr MetadataWithResourceVersion
fetchMetadataAndResourceVersionFromCatalog = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
       SELECT metadata, resource_version from hdb_catalog.hdb_metadata
    |]
      ()
      True
  uncurry MetadataWithResourceVersion <$> case rows of
    [] -> pure (emptyMetadata, initialResourceVersion)
    [(PG.ViaJSON metadata, resourceVersion)] -> pure (metadata, MetadataResourceVersion resourceVersion)
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataResourceVersionFromCatalog :: PG.TxE QErr MetadataResourceVersion
fetchMetadataResourceVersionFromCatalog = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
       SELECT resource_version from hdb_catalog.hdb_metadata
    |]
      ()
      True
  case rows of
    [] -> pure initialResourceVersion
    [Identity resourceVersion] -> pure (MetadataResourceVersion resourceVersion)
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataNotificationsFromCatalog :: MetadataResourceVersion -> InstanceId -> PG.TxE QErr [(MetadataResourceVersion, CacheInvalidations)]
fetchMetadataNotificationsFromCatalog (MetadataResourceVersion resourceVersion) instanceId = do
  fmap (bimap MetadataResourceVersion PG.getViaJSON)
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
         SELECT resource_version, notification
         FROM hdb_catalog.hdb_schema_notifications
         WHERE resource_version > $1 AND instance_id != ($2::uuid)
      |]
      (resourceVersion, instanceId)
      True

-- Used to increment metadata version when no other changes are required
bumpMetadataVersionInCatalog :: PG.TxE QErr ()
bumpMetadataVersionInCatalog = do
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      UPDATE hdb_catalog.hdb_metadata
      SET resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      |]
    ()
    True

insertMetadataInCatalog :: Metadata -> PG.TxE QErr ()
insertMetadataInCatalog metadata =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
    INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
    VALUES (1, $1::json)
    |]
    (Identity $ PG.ViaJSON metadata)
    True

-- | Check that the specified resource version matches the currently stored one, and...
--
-- - If so: Update the metadata and bump the version
-- - If not: Throw a 409 error
setMetadataInCatalog :: MetadataResourceVersion -> Metadata -> PG.TxE QErr MetadataResourceVersion
setMetadataInCatalog resourceVersion metadata = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
    VALUES (1, $1::json)
    ON CONFLICT (id) DO UPDATE SET
      metadata = $1::json,
      resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      WHERE hdb_catalog.hdb_metadata.resource_version = $2
    RETURNING resource_version
    |]
      (PG.ViaJSON metadata, getMetadataResourceVersion resourceVersion)
      True
  case rows of
    [] -> throw409 $ "metadata resource version referenced (" <> tshow (getMetadataResourceVersion resourceVersion) <> ") did not match current version"
    [Identity newResourceVersion] -> pure $ MetadataResourceVersion newResourceVersion
    _ -> throw500 "multiple rows in hdb_metadata table"
