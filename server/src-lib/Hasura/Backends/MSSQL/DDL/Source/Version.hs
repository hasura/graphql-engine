{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.MSSQL.DDL.Source.Version
  ( SourceCatalogVersion,
    initialSourceCatalogVersion,
    latestSourceCatalogVersion,
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
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (MSSQL))
import Hasura.Server.Migrate.Version qualified as Version

type SourceCatalogVersion = Version.SourceCatalogVersion 'MSSQL

initialSourceCatalogVersion :: SourceCatalogVersion
initialSourceCatalogVersion = Version.SourceCatalogVersion 1

latestSourceCatalogVersion :: SourceCatalogVersion
latestSourceCatalogVersion = Version.SourceCatalogVersion 4

previousSourceCatalogVersions :: [SourceCatalogVersion]
previousSourceCatalogVersions = [initialSourceCatalogVersion .. pred latestSourceCatalogVersion]

setSourceCatalogVersion :: (MonadMSSQLTx m) => SourceCatalogVersion -> m ()
setSourceCatalogVersion (Version.SourceCatalogVersion version) =
  liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler setSourceCatalogVersionQuery
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

getSourceCatalogVersion :: (MonadMSSQLTx m) => m SourceCatalogVersion
getSourceCatalogVersion =
  Version.SourceCatalogVersion
    <$> liftMSSQLTx (singleRowQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |])
