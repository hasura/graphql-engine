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
    prepareCatalog,
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
import Hasura.Backends.MSSQL.DDL.EventTrigger
import Hasura.Backends.MSSQL.DDL.Source.Version
import Hasura.Backends.MSSQL.Meta
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Backends.MSSQL.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend (BackendConfig)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.Backend
import Hasura.Server.Migrate.Version
import Language.Haskell.TH.Lib qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.Shakespeare.Text qualified as ST

resolveSourceConfig ::
  (MonadIO m, MonadResolveSource m) =>
  SourceName ->
  MSSQLConnConfiguration ->
  BackendSourceKind 'MSSQL ->
  BackendConfig 'MSSQL ->
  Env.Environment ->
  m (Either QErr MSSQLSourceConfig)
resolveSourceConfig name config _backendKind _backendConfig _env = runExceptT do
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

doesTableExist :: MonadMSSQLTx m => TableName -> m Bool
doesTableExist tableName = do
  liftMSSQLTx $
    Tx.singleRowQueryE
      HGE.defaultMSSQLTxErrorHandler
      [ODBC.sql|
        SELECT CAST (
          CASE
            WHEN (Select OBJECT_ID($qualifiedTable)) IS NOT NULL
              THEN 1
            ELSE 0
          END
        AS BIT)
      |]
  where
    qualifiedTable = qualifyTableName tableName

-- | Initialise catalog tables for a source, including those required by the event delivery subsystem.
prepareCatalog ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  ExceptT QErr m RecreateEventTriggers
prepareCatalog sourceConfig = mssqlRunReadWrite (_mscExecCtx sourceConfig) do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist $ TableName "event_log" "hdb_catalog"
  sourceVersionTableExist <- doesTableExist $ TableName "hdb_source_catalog_version" "hdb_catalog"
  if
      -- Fresh database
      | not hdbCatalogExist -> liftMSSQLTx do
        unitQueryE HGE.defaultMSSQLTxErrorHandler "CREATE SCHEMA hdb_catalog"
        initSourceCatalog
        return RETDoNothing
      -- Only 'hdb_catalog' schema defined
      | not (sourceVersionTableExist || eventLogTableExist) -> do
        liftMSSQLTx initSourceCatalog
        return RETDoNothing
      | otherwise -> migrateSourceCatalog
  where
    initSourceCatalog = do
      unitQueryE HGE.defaultMSSQLTxErrorHandler $(makeRelativeToProject "src-rsr/mssql/init_mssql_source.sql" >>= ODBC.sqlFile)
      setSourceCatalogVersion latestSourceCatalogVersion

dropSourceCatalog :: MonadMSSQLTx m => m ()
dropSourceCatalog = do
  let sql = $(makeRelativeToProject "src-rsr/mssql/drop_mssql_source.sql" >>= ODBC.sqlFile)
  liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler sql

migrateSourceCatalog :: MonadMSSQLTx m => m RecreateEventTriggers
migrateSourceCatalog =
  getSourceCatalogVersion >>= migrateSourceCatalogFrom

migrateSourceCatalogFrom :: MonadMSSQLTx m => CatalogVersion -> m RecreateEventTriggers
migrateSourceCatalogFrom prevVersion
  | prevVersion == latestSourceCatalogVersion = pure RETDoNothing
  | [] <- neededMigrations =
    throw400 NotSupported $
      "Expected source catalog version <= "
        <> latestSourceCatalogVersionText
        <> ", but the current version is "
        <> (tshow prevVersion)
  | otherwise = do
    liftMSSQLTx $ traverse_ snd neededMigrations
    setSourceCatalogVersion latestSourceCatalogVersion
    pure RETRecreate
  where
    neededMigrations =
      dropWhile ((/= prevVersion) . fst) sourceMigrations

sourceMigrations :: [(CatalogVersion, TxE QErr ())]
sourceMigrations =
  $( let migrationFromFile from =
           let to = succ from
               path = "src-rsr/mssql_source_migrations/" <> show from <> "_to_" <> show to <> ".sql"
            in [|multiRowQueryE defaultTxErrorHandler $(makeRelativeToProject path >>= ST.stextFile)|]

         migrationsFromFile = map $ \from ->
           [|($(TH.lift $ from), $(migrationFromFile from))|]
      in TH.listE $ migrationsFromFile previousSourceCatalogVersions
   )
