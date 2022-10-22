{-# LANGUAGE QuasiQuotes #-}

-- | Postgres DDL Source Version
--
-- Deals with catalog version - used by 'Hasura.Backends.Postgres.DDL.Source'.
module Hasura.Backends.Postgres.DDL.Source.Version
  ( SourceCatalogVersion,
    initialSourceCatalogVersion,
    latestSourceCatalogVersion,
    previousSourceCatalogVersions,
    getSourceCatalogVersion,
    setSourceCatalogVersion,
  )
where

import Data.Text qualified as T
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind)
import Hasura.Server.Migrate.Version qualified as Version

type SourceCatalogVersion (pgKind :: PostgresKind) = Version.SourceCatalogVersion ('Postgres pgKind)

initialSourceCatalogVersion :: SourceCatalogVersion pgKind
initialSourceCatalogVersion = Version.SourceCatalogVersion 0

latestSourceCatalogVersion :: SourceCatalogVersion pgKind
latestSourceCatalogVersion = Version.SourceCatalogVersion 2

previousSourceCatalogVersions :: [SourceCatalogVersion pgKind]
previousSourceCatalogVersions = [initialSourceCatalogVersion .. pred latestSourceCatalogVersion]

setSourceCatalogVersion :: MonadTx m => m ()
setSourceCatalogVersion =
  liftTx $
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
        INSERT INTO hdb_catalog.hdb_source_catalog_version(version, upgraded_on)
          VALUES ($1, NOW())
        ON CONFLICT ((version IS NOT NULL))
        DO UPDATE SET version = $1, upgraded_on = NOW()
      |]
      (Identity (tshow latestSourceCatalogVersion))
      False

getSourceCatalogVersion :: MonadTx m => m (SourceCatalogVersion postgres)
getSourceCatalogVersion = do
  versionText <-
    liftTx $
      runIdentity . Q.getRow
        <$> Q.withQE
          defaultTxErrorHandler
          [Q.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |]
          ()
          False
  readEither (T.unpack versionText) `onLeft` (throw500 . (("Invalid source catalog version in the metadata: " <>) . T.pack))
