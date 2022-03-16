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
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
  ( MetadataResourceVersion (..),
    initialResourceVersion,
  )
import Hasura.RQL.Types.SchemaCache.Build (CacheInvalidations)
import Hasura.Server.Types (InstanceId (..))

fetchMetadataFromCatalog :: Q.TxE QErr Metadata
fetchMetadataFromCatalog = do
  rows <-
    Q.withQE
      defaultTxErrorHandler
      [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata
    |]
      ()
      True
  case rows of
    [] -> pure emptyMetadata
    [Identity (Q.AltJ metadata)] -> pure metadata
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataAndResourceVersionFromCatalog :: Q.TxE QErr (Metadata, MetadataResourceVersion)
fetchMetadataAndResourceVersionFromCatalog = do
  rows <-
    Q.withQE
      defaultTxErrorHandler
      [Q.sql|
       SELECT metadata, resource_version from hdb_catalog.hdb_metadata
    |]
      ()
      True
  case rows of
    [] -> pure (emptyMetadata, initialResourceVersion)
    [(Q.AltJ metadata, resourceVersion)] -> pure (metadata, MetadataResourceVersion resourceVersion)
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataResourceVersionFromCatalog :: Q.TxE QErr MetadataResourceVersion
fetchMetadataResourceVersionFromCatalog = do
  rows <-
    Q.withQE
      defaultTxErrorHandler
      [Q.sql|
       SELECT resource_version from hdb_catalog.hdb_metadata
    |]
      ()
      True
  case rows of
    [] -> pure initialResourceVersion
    [Identity resourceVersion] -> pure (MetadataResourceVersion resourceVersion)
    _ -> throw500 "multiple rows in hdb_metadata table"

fetchMetadataNotificationsFromCatalog :: MetadataResourceVersion -> InstanceId -> Q.TxE QErr [(MetadataResourceVersion, CacheInvalidations)]
fetchMetadataNotificationsFromCatalog (MetadataResourceVersion resourceVersion) instanceId = do
  fmap (bimap MetadataResourceVersion Q.getAltJ)
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
         SELECT resource_version, notification
         FROM hdb_catalog.hdb_schema_notifications
         WHERE resource_version > $1 AND instance_id != ($2::uuid)
      |]
      (resourceVersion, instanceId)
      True

-- Used to increment metadata version when no other changes are required
bumpMetadataVersionInCatalog :: Q.TxE QErr ()
bumpMetadataVersionInCatalog = do
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
      UPDATE hdb_catalog.hdb_metadata
      SET resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      |]
    ()
    True

insertMetadataInCatalog :: Metadata -> Q.TxE QErr ()
insertMetadataInCatalog metadata =
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
    INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
    VALUES (1, $1::json)
    |]
    (Identity $ Q.AltJ metadata)
    True

-- | Check that the specified resource version matches the currently stored one, and...
--
-- - If so: Update the metadata and bump the version
-- - If not: Throw a 409 error
setMetadataInCatalog :: MetadataResourceVersion -> Metadata -> Q.TxE QErr MetadataResourceVersion
setMetadataInCatalog resourceVersion metadata = do
  rows <-
    Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    INSERT INTO hdb_catalog.hdb_metadata(id, metadata)
    VALUES (1, $1::json)
    ON CONFLICT (id) DO UPDATE SET
      metadata = $1::json,
      resource_version = hdb_catalog.hdb_metadata.resource_version + 1
      WHERE hdb_catalog.hdb_metadata.resource_version = $2
    RETURNING resource_version
    |]
      (Q.AltJ metadata, getMetadataResourceVersion resourceVersion)
      True
  case rows of
    [] -> throw409 $ "metadata resource version referenced (" <> tshow (getMetadataResourceVersion resourceVersion) <> ") did not match current version"
    [Identity newResourceVersion] -> pure $ MetadataResourceVersion newResourceVersion
    _ -> throw500 "multiple rows in hdb_metadata table"
