module Hasura.Backends.MSSQL.DDL.Source
  ( resolveSourceConfig
  , resolveDatabaseMetadata
  , postDropSourceHook
  )
where

import           Hasura.Prelude

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Meta
import           Hasura.Base.Error
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Source
import           Hasura.SQL.Backend

resolveSourceConfig
  :: (MonadIO m)
  => SourceName
  -> MSSQLConnConfiguration
  -> m (Either QErr MSSQLSourceConfig)
resolveSourceConfig _name (MSSQLConnConfiguration connInfo) = runExceptT do
  (connString, mssqlPool) <- createMSSQLPool connInfo
  pure $ MSSQLSourceConfig connString mssqlPool

resolveDatabaseMetadata
  :: (MonadIO m)
  => MSSQLSourceConfig
  -> m (Either QErr (ResolvedSource 'MSSQL))
resolveDatabaseMetadata config = runExceptT do
  dbTablesMetadata <- loadDBMetadata pool
  pure $ ResolvedSource config dbTablesMetadata mempty mempty
  where
    MSSQLSourceConfig _connString pool = config

postDropSourceHook
  :: (MonadIO m)
  => MSSQLSourceConfig -> m ()
postDropSourceHook (MSSQLSourceConfig _ pool) =
  -- Close the connection
  liftIO $ drainMSSQLPool pool
