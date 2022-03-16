{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    initCatalogForSource,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Database.MSSQL.Transaction
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.SQLServer
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.DDL.Source.Version
import Hasura.Backends.MSSQL.Meta
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Backends.MSSQL.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
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
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  m ()
postDropSourceHook (MSSQLSourceConfig _ mssqlExecCtx) = do
  _ <- runExceptT $ mssqlRunReadWrite mssqlExecCtx dropSourceCatalog
  -- Close the connection
  liftIO $ mssqlDestroyConn mssqlExecCtx

doesSchemaExist :: MonadMSSQLTx m => SchemaName -> m Bool
doesSchemaExist (SchemaName schemaName) = do
  liftMSSQLTx $
    Tx.singleRowQueryE
      HGE.defaultMSSQLTxErrorHandler
      [ODBC.sql|
      SELECT CAST (
        CASE
          WHEN EXISTS( SELECT 1 FROM sys.schemas WHERE name = $schemaName )
            THEN 1 
          ELSE 0 
        END
      AS BIT)
    |]

-- | Initialise catalog tables for a source, including those required by the event delivery subsystem.
initCatalogForSource :: MonadMSSQLTx m => m RecreateEventTriggers
initCatalogForSource = do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"

  if
      -- Fresh database
      | not hdbCatalogExist -> liftMSSQLTx do
        unitQueryE HGE.defaultMSSQLTxErrorHandler "CREATE SCHEMA hdb_catalog"
        initSourceCatalog
        return RETDoNothing
      -- TODO: When we need to make any changes to the source catalog, we'll have to introduce code which which will migrate
      -- from one source catalog version to the next one
      | otherwise -> pure RETDoNothing
  where
    initSourceCatalog = do
      unitQueryE HGE.defaultMSSQLTxErrorHandler $(makeRelativeToProject "src-rsr/init_mssql_source.sql" >>= ODBC.sqlFile)
      setSourceCatalogVersion latestSourceCatalogVersion

dropSourceCatalog :: MonadMSSQLTx m => m ()
dropSourceCatalog = do
  let sql = $(makeRelativeToProject "src-rsr/drop_mssql_source.sql" >>= ODBC.sqlFile)
  liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler sql
