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
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Lazy qualified as LT
import Database.MSSQL.Transaction
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.SQLServer
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.DDL.EventTrigger
import Hasura.Backends.MSSQL.DDL.Source.Version
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Meta
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Backends.MSSQL.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend (BackendConfig)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
import Hasura.RQL.Types.Source
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (..))
import Hasura.Server.Migrate.Version qualified as Version
import Hasura.Table.Cache
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
  manager ->
  m (Either QErr MSSQLSourceConfig)
resolveSourceConfig name config _backendKind _backendConfig env _manager = runExceptT do
  sourceResolver <- getMSSQLSourceResolver
  liftEitherM $ liftIO $ sourceResolver env name config

resolveDatabaseMetadata ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  m (Either QErr (DBObjectsIntrospection 'MSSQL))
resolveDatabaseMetadata config = runExceptT do
  dbTablesMetadata <- mssqlRunReadOnly mssqlExecCtx $ loadDBMetadata
  pure $ DBObjectsIntrospection dbTablesMetadata mempty mempty mempty
  where
    MSSQLSourceConfig _connString mssqlExecCtx _numReadReplicas = config

postDropSourceHook ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  TableEventTriggers 'MSSQL ->
  m ()
postDropSourceHook (MSSQLSourceConfig _ mssqlExecCtx _) tableTriggersMap = do
  -- The SQL triggers for MSSQL source are created within the schema of the table,
  -- and is not associated with 'hdb_catalog' schema. Thus only deleting the
  -- 'hdb_catalog' schema is not sufficient, since it will still leave the SQL
  -- triggers within the table schema.
  --
  -- This causes problems, whenever the next insert/delete/update operation occurs
  -- the SQL triggers will try to unsuccessfully insert event_log to the nonexistent
  -- 'hdb_catalog.event_log' table. The left over SQL triggers thus stops any
  -- operation that will happen on the table.
  --
  -- Hence we first delete all the related Hasura SQL triggers and then drop the
  -- 'hdb_catalog' schema.
  for_ (HashMap.toList tableTriggersMap) $ \(_table@(TableName _tableName schema), triggers) ->
    for_ triggers $ \triggerName ->
      liftIO $ runExceptT $ mssqlRunReadWrite mssqlExecCtx (dropTriggerQ triggerName schema)
  _ <- runExceptT $ mssqlRunReadWrite mssqlExecCtx dropSourceCatalog
  -- Close the connection
  liftIO $ mssqlDestroyConn mssqlExecCtx

doesSchemaExist :: (MonadMSSQLTx m) => SchemaName -> m Bool
doesSchemaExist (SchemaName schemaName) = do
  liftMSSQLTx
    $ Tx.singleRowQueryE
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

doesTableExist :: (MonadMSSQLTx m) => TableName -> m Bool
doesTableExist tableName = do
  liftMSSQLTx
    $ Tx.singleRowQueryE
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
  ExceptT QErr m (RecreateEventTriggers, SourceCatalogMigrationState)
prepareCatalog sourceConfig = mssqlRunSerializableTx (_mscExecCtx sourceConfig) do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist $ TableName "event_log" "hdb_catalog"
  sourceVersionTableExist <- doesTableExist $ TableName "hdb_source_catalog_version" "hdb_catalog"
  if
    -- Fresh database
    | not hdbCatalogExist -> liftMSSQLTx do
        unitQueryE HGE.defaultMSSQLTxErrorHandler "CREATE SCHEMA hdb_catalog"
        initSourceCatalog
        return (RETDoNothing, Version.SCMSInitialized $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
    -- Only 'hdb_catalog' schema defined
    | not (sourceVersionTableExist || eventLogTableExist) -> do
        liftMSSQLTx initSourceCatalog
        return (RETDoNothing, Version.SCMSInitialized $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
    | otherwise -> migrateSourceCatalog
  where
    initSourceCatalog = do
      unitQueryE HGE.defaultMSSQLTxErrorHandler $(makeRelativeToProject "src-rsr/mssql/init_mssql_source.sql" >>= ODBC.sqlFile)
      setSourceCatalogVersion latestSourceCatalogVersion

dropSourceCatalog :: (MonadMSSQLTx m) => m ()
dropSourceCatalog = do
  let sql = $(makeRelativeToProject "src-rsr/mssql/drop_mssql_source.sql" >>= ODBC.sqlFile)
  liftMSSQLTx $ unitQueryE HGE.defaultMSSQLTxErrorHandler sql

migrateSourceCatalog :: (MonadMSSQLTx m) => m (RecreateEventTriggers, SourceCatalogMigrationState)
migrateSourceCatalog =
  getSourceCatalogVersion >>= migrateSourceCatalogFrom

migrateSourceCatalogFrom :: (MonadMSSQLTx m) => SourceCatalogVersion -> m (RecreateEventTriggers, SourceCatalogMigrationState)
migrateSourceCatalogFrom prevVersion
  | prevVersion == latestSourceCatalogVersion = pure (RETDoNothing, SCMSNothingToDo $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
  | [] <- neededMigrations =
      throw400 NotSupported
        $ "Expected source catalog version <= "
        <> tshow latestSourceCatalogVersion
        <> ", but the current version is "
        <> tshow prevVersion
  | otherwise = do
      liftMSSQLTx $ traverse_ snd neededMigrations
      setSourceCatalogVersion latestSourceCatalogVersion
      pure (RETRecreate, SCMSMigratedTo (Version.unSourceCatalogVersion prevVersion) (Version.unSourceCatalogVersion latestSourceCatalogVersion))
  where
    neededMigrations =
      dropWhile ((/= prevVersion) . fst) sourceMigrations

sourceMigrations :: [(SourceCatalogVersion, TxE QErr [Text])]
sourceMigrations =
  $( let migrationFromFile from =
           let to = succ from
               path = "src-rsr/mssql/mssql_source_migrations/" <> show from <> "_to_" <> show to <> ".sql"
            in do
                 [|(multiRowQueryE HGE.defaultMSSQLTxErrorHandler $ rawUnescapedText . LT.toStrict $ $(makeRelativeToProject path >>= ST.stextFile))|]

         migrationsFromFile = map $ \from ->
           [|($(TH.lift $ from), $(migrationFromFile from))|]
      in TH.listE $ migrationsFromFile previousSourceCatalogVersions
   )
