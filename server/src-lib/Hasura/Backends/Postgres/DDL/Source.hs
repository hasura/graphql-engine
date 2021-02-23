module Hasura.Backends.Postgres.DDL.Source
  ( resolveSourceConfig
  , postDropSourceHook
  , resolveDatabaseMetadata
  ) where

import qualified Data.HashMap.Strict                 as Map
import qualified Database.PG.Query                   as Q
import qualified Language.Haskell.TH.Lib             as TH
import qualified Language.Haskell.TH.Syntax          as TH

import           Control.Lens                        hiding (from, index, op, to, (.=))
import           Control.Monad.Trans.Control         (MonadBaseControl)

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Prelude
import           Hasura.RQL.Types.Backend            (SourceConfig)
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Source
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.Server.Migrate.Internal

resolveSourceConfig
  :: (MonadIO m, MonadResolveSource m)
  => SourceName -> PostgresConnConfiguration -> m (Either QErr (SourceConfig 'Postgres))
resolveSourceConfig name config = runExceptT do
  sourceResolver <- getSourceResolver
  liftEitherM $ liftIO $ sourceResolver name config

resolveDatabaseMetadata
  :: (MonadIO m, MonadBaseControl IO m)
  => SourceConfig 'Postgres -> m (Either QErr (ResolvedSource 'Postgres))
resolveDatabaseMetadata sourceConfig = runExceptT do
  (tablesMeta, functionsMeta, pgScalars) <- runLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite $ do
    initSource
    tablesMeta    <- fetchTableMetadata
    functionsMeta <- fetchFunctionMetadata
    pgScalars     <- fetchPgScalars
    pure (tablesMeta, functionsMeta, pgScalars)
  pure $ ResolvedSource sourceConfig tablesMeta functionsMeta pgScalars

initSource :: MonadTx m => m ()
initSource = do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
  sourceVersionTableExist <- doesTableExist "hdb_catalog" "hdb_source_catalog_version"
     -- Fresh database
  if | not hdbCatalogExist -> liftTx do
        Q.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
        enablePgcryptoExtension
        initPgSourceCatalog
     -- Only 'hdb_catalog' schema defined
     | not sourceVersionTableExist && not eventLogTableExist ->
        liftTx initPgSourceCatalog
     -- Source is initialised by pre multisource support servers
     | not sourceVersionTableExist && eventLogTableExist -> do
       -- Update the Source Catalog to v43 to include the new migration
       -- changes. Skipping this step will result in errors.
        currCatalogVersion <- getCatalogVersion
        migrateTo43 currCatalogVersion
        liftTx createVersionTable
     | otherwise -> migrateSourceCatalog
  where
    initPgSourceCatalog = do
      () <- Q.multiQE defaultTxErrorHandler $(Q.sqlFromFile "src-rsr/init_pg_source.sql")
      setSourceCatalogVersion

    createVersionTable = do
      () <- Q.multiQE defaultTxErrorHandler
        [Q.sql|
           CREATE TABLE hdb_catalog.hdb_source_catalog_version(
             version TEXT NOT NULL,
             upgraded_on TIMESTAMPTZ NOT NULL
           );

           CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
           ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));
        |]
      setSourceCatalogVersion

    migrateSourceCatalog = do
      version <- getSourceCatalogVersion
      case version of
        "1" -> pure ()
        _   -> throw500 $ "unexpected source catalog version: " <> version

    migrateTo43 prevVersion = do
      let neededMigrations = dropWhile ((/= prevVersion) . fst) upMigrationsUntil43
      traverse_ snd neededMigrations

-- Upgrade the hdb_catalog schema to v43
upMigrationsUntil43 :: MonadTx m => [(Text, m ())]
upMigrationsUntil43 =
    $(let migrationFromFile from to =
            let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
             in [| runTx $(Q.sqlFromFile path) |]

          migrationsFromFile = map $ \(to :: Integer) ->
            let from = to - 1
            in [| ( $(TH.lift $ tshow from)
                  , $(migrationFromFile (show from) (show to))
                  ) |]
      in TH.listE
        -- version 0.8 is the only non-integral catalog version
        $  [| ("0.8", $(migrationFromFile "08" "1")) |]
        :  migrationsFromFile [2..3]
        ++ [| ("3", from3To4) |]
        :  migrationsFromFile [5..43]
     )

currentSourceCatalogVersion :: Text
currentSourceCatalogVersion = "1"

setSourceCatalogVersion :: MonadTx m => m ()
setSourceCatalogVersion = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
  INSERT INTO hdb_catalog.hdb_source_catalog_version(version, upgraded_on)
    VALUES ($1, NOW())
   ON CONFLICT ((version IS NOT NULL))
   DO UPDATE SET version = $1, upgraded_on = NOW()
  |] (Identity currentSourceCatalogVersion) False

getSourceCatalogVersion :: MonadTx m => m Text
getSourceCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  [Q.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |] () False

-- | Fetch Postgres metadata of all user tables
fetchTableMetadata :: (MonadTx m) => m (DBTablesMetadata 'Postgres)
fetchTableMetadata = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_table_metadata.sql") () True
  pure $ Map.fromList $ flip map results $
    \(schema, table, Q.AltJ info) -> (QualifiedObject schema table, info)

-- | Fetch Postgres metadata for all user functions
fetchFunctionMetadata :: (MonadTx m) => m (DBFunctionsMetadata 'Postgres)
fetchFunctionMetadata = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_function_metadata.sql") () True
  pure $ Map.fromList $ flip map results $
    \(schema, table, Q.AltJ infos) -> (QualifiedObject schema table, infos)

-- | Fetch all scalar types from Postgres
fetchPgScalars :: MonadTx m => m (HashSet PGScalarType)
fetchPgScalars =
  liftTx $ Q.getAltJ . runIdentity . Q.getRow
  <$> Q.withQE defaultTxErrorHandler
  [Q.sql|
    SELECT coalesce(json_agg(typname), '[]')
    FROM pg_catalog.pg_type where typtype = 'b'
   |] () True

-- | Clean source database after dropping in metadata
postDropSourceHook
  :: (MonadIO m, MonadError QErr m, MonadBaseControl IO m)
  => PGSourceConfig -> m ()
postDropSourceHook sourceConfig = do
  -- Clean traces of Hasura in source database
  liftEitherM $ runPgSourceWriteTx sourceConfig $ do
    hdbMetadataTableExist <- doesTableExist "hdb_catalog" "hdb_metadata"
    eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
        -- If "hdb_metadata" and "event_log" tables found in the "hdb_catalog" schema
        -- then this infers the source is being used as default potgres source (--database-url option).
        -- In this case don't drop any thing in the catalog schema.
    if | hdbMetadataTableExist && eventLogTableExist -> pure ()
       -- Otherwise, if only "hdb_metadata" table exist, then this infers the source is
       -- being used as metadata storage (--metadata-database-url option). In this case
       -- drop only source related tables and not "hdb_catalog" schema
       | hdbMetadataTableExist ->
         Q.multiQE defaultTxErrorHandler $(Q.sqlFromFile "src-rsr/drop_pg_source.sql")
       -- Otherwise, drop "hdb_catalog" schema.
       | otherwise -> dropHdbCatalogSchema

  -- Destory postgres source connection
  liftIO $ _pecDestroyConn $ _pscExecCtx sourceConfig
