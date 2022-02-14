-- | MSSQL Source
--
-- Implements the Source related methods of the
-- 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata' type class
-- for the MSSQL backend, which provides an interface for identifying the
-- MSSQL database instance (source) and manipulate it.
--
-- The actual instance is defined in "Hasura.Backends.MSSQL.Instances.Metadata".
module Hasura.Backends.MSSQL.DDL.Source
  ( resolveSourceConfig,
    resolveDatabaseMetadata,
    postDropSourceHook,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Environment qualified as Env
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Meta
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.Backend

resolveSourceConfig ::
  (MonadIO m, MonadResolveSource m) =>
  SourceName ->
  MSSQLConnConfiguration ->
  Env.Environment ->
  m (Either QErr MSSQLSourceConfig)
resolveSourceConfig name config _env = runExceptT do
  sourceResolver <- getMSSQLSourceResolver
  liftEitherM $ liftIO $ sourceResolver name config

resolveDatabaseMetadata ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  SourceTypeCustomization ->
  m (Either QErr (ResolvedSource 'MSSQL))
resolveDatabaseMetadata config customization = runExceptT do
  dbTablesMetadata <- mssqlRunReadOnly mssqlExecCtx $ loadDBMetadata
  pure $ ResolvedSource config customization dbTablesMetadata mempty mempty
  where
    MSSQLSourceConfig _connString mssqlExecCtx = config

postDropSourceHook ::
  (MonadIO m) =>
  MSSQLSourceConfig ->
  m ()
postDropSourceHook (MSSQLSourceConfig _ mssqlExecCtx) = do
  -- Close the connection
  liftIO $ mssqlDestroyConn mssqlExecCtx
