{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.MSSQL.Schema.Introspection
  ( listAllTables,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate (i)
import Database.MSSQL.Transaction (multiRowQueryE)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection (runMSSQLSourceWriteTx)
import Hasura.Backends.MSSQL.SQL.Error (defaultMSSQLTxErrorHandler)
import Hasura.Backends.MSSQL.Types (SchemaName (..), TableName (..))
import Hasura.Base.Error (QErr, prefixQErr)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (MSSQL))
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.Metadata (MetadataM)
import Hasura.RQL.Types.SchemaCache (CacheRM, askSourceConfig)

-- | List all tables, tracked or untracked, on a given data source.
listAllTables :: (CacheRM m, MetadataM m, MonadBaseControl IO m, MonadError QErr m, MonadIO m) => SourceName -> m [TableName]
listAllTables sourceName = do
  sourceConfig <- askSourceConfig @'MSSQL sourceName

  let listAllTablesSql :: ODBC.Query
      listAllTablesSql =
        [i|
          select table_name, table_schema
          from information_schema.tables
          where table_schema not in (
            'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
          );
        |]

  results :: [(Text, Text)] <-
    runMSSQLSourceWriteTx sourceConfig (multiRowQueryE defaultMSSQLTxErrorHandler listAllTablesSql)
      `onLeftM` \err -> throwError (prefixQErr "failed to fetch source tables: " err)

  pure [TableName {..} | (tableName, SchemaName -> tableSchema) <- results]
