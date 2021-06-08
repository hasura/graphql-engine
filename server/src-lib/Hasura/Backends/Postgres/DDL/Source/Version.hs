module Hasura.Backends.Postgres.DDL.Source.Version where

import           Hasura.Prelude

import qualified Database.PG.Query                   as Q

import           Hasura.Backends.Postgres.Connection


latestSourceCatalogVersion :: Integer
latestSourceCatalogVersion = 2

latestSourceCatalogVersionText :: Text
latestSourceCatalogVersionText = tshow latestSourceCatalogVersion

setSourceCatalogVersion :: MonadTx m => m ()
setSourceCatalogVersion = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
  INSERT INTO hdb_catalog.hdb_source_catalog_version(version, upgraded_on)
    VALUES ($1, NOW())
   ON CONFLICT ((version IS NOT NULL))
   DO UPDATE SET version = $1, upgraded_on = NOW()
  |] (Identity latestSourceCatalogVersionText) False

getSourceCatalogVersion :: MonadTx m => m Text
getSourceCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  [Q.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |] () False
