{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.MSSQL.DDL.Source.Version
  ( latestSourceCatalogVersion,
    setSourceCatalogVersion,
    getSourceCatalogVersion,
    latestSourceCatalogVersionText,
  )
where

import Database.MSSQL.Transaction
import Database.ODBC.SQLServer
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection (MonadMSSQLTx (..))
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Prelude

latestSourceCatalogVersion :: Int
latestSourceCatalogVersion = 1

latestSourceCatalogVersionText :: Text
latestSourceCatalogVersionText = tshow latestSourceCatalogVersion

setSourceCatalogVersion :: MonadMSSQLTx m => Int -> m ()
setSourceCatalogVersion version = liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler setSourceCatalogVersionQuery
  where
    setSourceCatalogVersionQuery = [ODBC.sql| INSERT INTO hdb_catalog.hdb_source_catalog_version(version, upgraded_on) VALUES ($version, SYSDATETIMEOFFSET()) |]

getSourceCatalogVersion :: MonadMSSQLTx m => m Int
getSourceCatalogVersion =
  liftMSSQLTx $
    singleRowQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |]
