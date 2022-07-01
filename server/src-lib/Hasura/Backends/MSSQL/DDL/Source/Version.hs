{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.MSSQL.DDL.Source.Version
  ( latestSourceCatalogVersion,
    latestSourceCatalogVersionText,
    previousSourceCatalogVersions,
    setSourceCatalogVersion,
    getSourceCatalogVersion,
  )
where

import Database.MSSQL.Transaction
import Database.ODBC.SQLServer
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection (MonadMSSQLTx (..))
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.Server.Migrate.Version

latestSourceCatalogVersion :: CatalogVersion
latestSourceCatalogVersion = CatalogVersion 2

latestSourceCatalogVersionText :: Text
latestSourceCatalogVersionText = tshow latestSourceCatalogVersion

previousSourceCatalogVersions :: [CatalogVersion]
previousSourceCatalogVersions = [CatalogVersion 1 .. pred latestSourceCatalogVersion]

setSourceCatalogVersion :: MonadMSSQLTx m => CatalogVersion -> m ()
setSourceCatalogVersion (CatalogVersion version) = liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler setSourceCatalogVersionQuery
  where
    setSourceCatalogVersionQuery =
      [ODBC.sql| 
        BEGIN TRANSACTION
          IF EXISTS (select 1 from hdb_catalog.hdb_source_catalog_version WITH (UPDLOCK,SERIALIZABLE))
              BEGIN
                  UPDATE hdb_catalog.hdb_source_catalog_version
                  SET version = $version,  upgraded_on = SYSDATETIMEOFFSET()
              END
          ELSE
              BEGIN
                  INSERT INTO hdb_catalog.hdb_source_catalog_version (version, upgraded_on)
                  values ($version, SYSDATETIMEOFFSET())
              END
        COMMIT TRANSACTION
      |]
setSourceCatalogVersion CatalogVersion08 =
  throw500 "Cannot set the source catalog version to an unstable version."

getSourceCatalogVersion :: MonadMSSQLTx m => m CatalogVersion
getSourceCatalogVersion =
  CatalogVersion <$> liftMSSQLTx (singleRowQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |])
