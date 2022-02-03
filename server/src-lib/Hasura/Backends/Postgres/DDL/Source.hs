-- | Migrations for postgres source catalog
--
--    NOTE: Please have a look at the `server/documentation/migration-guidelines.md` before adding any new migration
--       if you haven't already looked at it
module Hasura.Backends.Postgres.DDL.Source
  ( ToMetadataFetchQuery,
    fetchPgScalars,
    fetchTableMetadata,
    fetchFunctionMetadata,
    initCatalogForSource,
    postDropSourceHook,
    resolveDatabaseMetadata,
    resolveSourceConfig,
    logPGSourceCatalogMigrationLockedQueries,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.TH
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (UTCTime)
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.DDL.Source.Version
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Hasura.Server.Migrate.Internal
import Hasura.Server.Types (EventingMode (..), MaintenanceMode (..), ReadOnlyMode (..))
import Language.Haskell.TH.Lib qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

-- | We differentiate the handling of metadata between Citus and Vanilla
-- Postgres because Citus imposes limitations on the types of joins that it
-- permits, which then limits the types of relations that we can track.
class ToMetadataFetchQuery (pgKind :: PostgresKind) where
  tableMetadata :: Q.Query

instance ToMetadataFetchQuery 'Vanilla where
  tableMetadata = $(makeRelativeToProject "src-rsr/pg_table_metadata.sql" >>= Q.sqlFromFile)

instance ToMetadataFetchQuery 'Citus where
  tableMetadata = $(makeRelativeToProject "src-rsr/citus_table_metadata.sql" >>= Q.sqlFromFile)

resolveSourceConfig ::
  (MonadIO m, MonadResolveSource m) =>
  SourceName ->
  PostgresConnConfiguration ->
  Env.Environment ->
  m (Either QErr (SourceConfig ('Postgres pgKind)))
resolveSourceConfig name config _env = runExceptT do
  sourceResolver <- getPGSourceResolver
  liftEitherM $ liftIO $ sourceResolver name config

-- | 'PGSourceLockQuery' is a data type which represents the contents of a single object of the
--   locked queries which are queried from the `pg_stat_activity`. See `logPGSourceCatalogMigrationLockedQueries`.
data PGSourceLockQuery = PGSourceLockQuery
  { _psqaQuery :: !Text,
    _psqaLockGranted :: !(Maybe Bool),
    _psqaLockMode :: !Text,
    _psqaTransactionStartTime :: !UTCTime,
    _psqaQueryStartTime :: !UTCTime,
    _psqaWaitEventType :: !Text,
    _psqaBlockingQuery :: !Text
  }

$(deriveJSON hasuraJSON ''PGSourceLockQuery)

instance ToEngineLog [PGSourceLockQuery] Hasura where
  toEngineLog resp = (LevelInfo, sourceCatalogMigrationLogType, toJSON resp)

newtype PGSourceLockQueryError = PGSourceLockQueryError QErr
  deriving (ToJSON)

instance ToEngineLog PGSourceLockQueryError Hasura where
  toEngineLog resp = (LevelError, sourceCatalogMigrationLogType, toJSON resp)

-- | 'logPGSourceCatalogMigrationLockedQueries' as the name suggests logs
--   the queries which are blocking in the database. This function is called
--   asynchronously from `initCatalogIfNeeded` while the source catalog is being
--   migrated.
--   NOTE: When there are no locking queries present in the database, nothing will be logged.
logPGSourceCatalogMigrationLockedQueries ::
  MonadIO m =>
  Logger Hasura ->
  PGSourceConfig ->
  m Void
logPGSourceCatalogMigrationLockedQueries logger sourceConfig = forever $ do
  dbStats <- liftIO $ runPgSourceReadTx sourceConfig fetchLockedQueriesTx
  case dbStats of
    Left err -> unLogger logger $ PGSourceLockQueryError err
    Right (val :: (Maybe [PGSourceLockQuery])) ->
      case val of
        Nothing -> pure ()
        Just [] -> pure ()
        Just val' -> liftIO $ unLogger logger $ val'
  liftIO $ sleep $ seconds 5
  where
    -- The blocking query in the below transaction is truncated to the first 20 characters because it may contain
    -- sensitive info.
    fetchLockedQueriesTx =
      (Q.getAltJ . runIdentity . Q.getRow)
        <$> Q.withQE
          defaultTxErrorHandler
          [Q.sql|
         SELECT COALESCE(json_agg(DISTINCT jsonb_build_object('query', psa.query, 'lock_granted', pl.granted, 'lock_mode', pl.mode, 'transaction_start_time', psa.xact_start, 'query_start_time', psa.query_start, 'wait_event_type', psa.wait_event_type, 'blocking_query', (SUBSTRING(blocking.query, 1, 20) || '...') )), '[]'::json)
         FROM     pg_stat_activity psa
         JOIN     pg_stat_activity blocking ON blocking.pid = ANY(pg_blocking_pids(psa.pid))
         LEFT JOIN pg_locks pl ON psa.pid = pl.pid
         WHERE    psa.query ILIKE '%hdb_catalog%' AND psa.wait_event_type IS NOT NULL
         AND      psa.query ILIKE any (array ['%create%', '%drop%', '%alter%']);
       |]
          ()
          False

resolveDatabaseMetadata ::
  forall pgKind m.
  (Backend ('Postgres pgKind), ToMetadataFetchQuery pgKind, MonadIO m, MonadBaseControl IO m) =>
  SourceConfig ('Postgres pgKind) ->
  SourceTypeCustomization ->
  m (Either QErr (ResolvedSource ('Postgres pgKind)))
resolveDatabaseMetadata sourceConfig sourceCustomization = runExceptT do
  runTx (_pscExecCtx sourceConfig) Q.ReadWrite ensureMetadataSupportingDefinitions

  (tablesMeta, functionsMeta, pgScalars) <- runTx (_pscExecCtx sourceConfig) Q.ReadOnly $ do
    tablesMeta <- fetchTableMetadata
    functionsMeta <- fetchFunctionMetadata
    pgScalars <- fetchPgScalars
    pure (tablesMeta, functionsMeta, pgScalars)
  pure $ ResolvedSource sourceConfig sourceCustomization tablesMeta functionsMeta pgScalars

-- | Initialise catalog tables for a source, including those required by the event delivery subsystem.
initCatalogForSource ::
  forall m. MonadTx m => MaintenanceMode -> EventingMode -> ReadOnlyMode -> UTCTime -> m RecreateEventTriggers
initCatalogForSource maintenanceMode eventingMode readOnlyMode migrationTime = do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
  sourceVersionTableExist <- doesTableExist "hdb_catalog" "hdb_source_catalog_version"

  if
      -- when safe mode is enabled, don't perform any migrations
      | readOnlyMode == ReadOnlyModeEnabled -> pure RETDoNothing
      -- when eventing mode is disabled, don't perform any migrations
      | eventingMode == EventingDisabled -> pure RETDoNothing
      -- when maintenance mode is enabled, don't perform any migrations
      | maintenanceMode == MaintenanceModeEnabled -> pure RETDoNothing
      -- Fresh database
      | not hdbCatalogExist -> liftTx do
        Q.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
        enablePgcryptoExtension
        initPgSourceCatalog
        return RETDoNothing
      -- Only 'hdb_catalog' schema defined
      | not sourceVersionTableExist && not eventLogTableExist -> do
        liftTx initPgSourceCatalog
        return RETDoNothing
      -- Source is initialised by pre multisource support servers
      | not sourceVersionTableExist && eventLogTableExist -> do
        -- Update the Source Catalog to v43 to include the new migration
        -- changes. Skipping this step will result in errors.
        currMetadataCatalogVersionFloat <- liftTx getCatalogVersion
        -- we migrate to the 43 version, which is the migration where
        -- metadata separation is introduced
        migrateTo43MetadataCatalog currMetadataCatalogVersionFloat
        liftTx createVersionTable
        -- Migrate the catalog from initial version i.e '0'
        migrateSourceCatalogFrom "0"
      | otherwise -> migrateSourceCatalog
  where
    initPgSourceCatalog = do
      () <- Q.multiQE defaultTxErrorHandler $(makeRelativeToProject "src-rsr/init_pg_source.sql" >>= Q.sqlFromFile)
      setSourceCatalogVersion

    createVersionTable = do
      () <-
        Q.multiQE
          defaultTxErrorHandler
          [Q.sql|
           CREATE TABLE hdb_catalog.hdb_source_catalog_version(
             version TEXT NOT NULL,
             upgraded_on TIMESTAMPTZ NOT NULL
           );

           CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
           ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));
        |]
      pure ()

    migrateTo43MetadataCatalog prevVersion = do
      let neededMigrations = dropWhile ((< prevVersion) . fst) upMigrationsUntil43
      case NE.nonEmpty neededMigrations of
        Just nonEmptyNeededMigrations -> do
          -- Migrations aren't empty. We need to update the catalog version after migrations
          liftTx $ traverse_ snd nonEmptyNeededMigrations
          setCatalogVersion "43" migrationTime
        Nothing ->
          -- No migrations exists, implies the database is migrated to latest metadata catalog version
          pure ()

-- NOTE (rakesh):
-- Down migrations for postgres sources is not supported in this PR. We need an
-- exhaustive discussion to make a call as I think, as of now, it is not
-- trivial. For metadata catalog migrations, we have a separate downgrade
-- command in the graphql-engine exe.
--
-- I can think of two ways:
--
--  - Just like downgrade, we need to have a new command path for downgrading
--  pg sources (command design should support other backends too,
--  graphql-engine source-downgrade postgres --to-catalog-version 1 --
--  downgrade all available pg sources to 1)
--  - Have an online documentation with necessary SQLs to help users to
--  downgrade pg sources themselves. Improve error message by referring the URL
--  to the documentation.

migrateSourceCatalog :: MonadTx m => m RecreateEventTriggers
migrateSourceCatalog =
  getSourceCatalogVersion >>= migrateSourceCatalogFrom

-- | `migrateSourceCatalogFrom` migrates the catalog from a lower to a higher version.
--    When there are any changes in the source catalog, then re-create the existing event
--    triggers in the metadata. This is done so that the event triggers be compatible with the
--    changes introduced in the newly added source catalog migrations. When the source is already
--    in the latest catalog version, we do nothing because nothing has changed w.r.t the source catalog
--    so recreating the event triggers will only be extraneous.
migrateSourceCatalogFrom :: (MonadTx m) => Text -> m RecreateEventTriggers
migrateSourceCatalogFrom prevVersion
  | prevVersion == latestSourceCatalogVersionText = pure RETDoNothing
  | [] <- neededMigrations =
    throw400 NotSupported $
      "Expected source catalog version <= "
        <> latestSourceCatalogVersionText
        <> ", but the current version is "
        <> prevVersion
  | otherwise = do
    liftTx $ traverse_ snd neededMigrations
    setSourceCatalogVersion
    pure RETRecreate
  where
    neededMigrations =
      dropWhile ((/= prevVersion) . fst) sourceMigrations

sourceMigrations :: [(Text, Q.TxE QErr ())]
sourceMigrations =
  $( let migrationFromFile from =
           let to = from + 1
               path = "src-rsr/pg_source_migrations/" <> show from <> "_to_" <> show to <> ".sql"
            in [|Q.multiQE defaultTxErrorHandler $(makeRelativeToProject path >>= Q.sqlFromFile)|]

         migrationsFromFile = map $ \(from :: Integer) ->
           [|($(TH.lift $ tshow from), $(migrationFromFile from))|]
      in TH.listE $ migrationsFromFile [0 .. (latestSourceCatalogVersion - 1)]
   )

-- Upgrade the hdb_catalog schema to v43 (Metadata catalog)
upMigrationsUntil43 :: [(Float, Q.TxE QErr ())]
upMigrationsUntil43 =
  $( let migrationFromFile from to =
           let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
            in [|Q.multiQE defaultTxErrorHandler $(makeRelativeToProject path >>= Q.sqlFromFile)|]

         migrationsFromFile = map $ \(to :: Float) ->
           let from = to - 1
            in [|
                 ( $(TH.lift from),
                   $(migrationFromFile (show (floor from :: Integer)) (show (floor to :: Integer)))
                 )
                 |]
      in TH.listE
         -- version 0.8 is the only non-integral catalog version
         -- The 41st migration which included only source catalog migration
         -- was introduced before metadata separation changes were introduced
         -- in the graphql-engine. Now the earlier 41st migration has been
         -- moved to source catalog migrations and the 41st up migration is removed
         -- entirely.
         $
           [|(0.8, $(migrationFromFile "08" "1"))|] :
           migrationsFromFile [2 .. 3]
             ++ [|(3, from3To4)|] :
           (migrationsFromFile [5 .. 40]) ++ migrationsFromFile [42 .. 43]
   )

-- | Ensure that the supporting definitions used in metadata fetching have been
-- loaded.
ensureMetadataSupportingDefinitions :: forall m. MonadTx m => m ()
ensureMetadataSupportingDefinitions = liftTx $ Q.multiQE defaultTxErrorHandler $(makeRelativeToProject "src-rsr/pg_metadata_lib.sql" >>= Q.sqlFromFile)

-- | Fetch Postgres metadata of all user tables
fetchTableMetadata ::
  forall pgKind m.
  (Backend ('Postgres pgKind), ToMetadataFetchQuery pgKind, MonadTx m) =>
  m (DBTablesMetadata ('Postgres pgKind))
fetchTableMetadata = do
  results <-
    liftTx $
      Q.withQE
        defaultTxErrorHandler
        (tableMetadata @pgKind)
        ()
        True
  pure $
    Map.fromList $
      flip map results $
        \(schema, table, Q.AltJ info) -> (QualifiedObject schema table, info)

-- | Fetch Postgres metadata for all user functions
fetchFunctionMetadata :: (MonadTx m) => m (DBFunctionsMetadata ('Postgres pgKind))
fetchFunctionMetadata = do
  results <-
    liftTx $
      Q.withQE
        defaultTxErrorHandler
        $(makeRelativeToProject "src-rsr/pg_function_metadata.sql" >>= Q.sqlFromFile)
        ()
        True
  pure $
    Map.fromList $
      flip map results $
        \(schema, table, Q.AltJ infos) -> (QualifiedObject schema table, infos)

-- | Fetch all scalar types from Postgres
fetchPgScalars :: MonadTx m => m (HashSet PGScalarType)
fetchPgScalars =
  liftTx $
    Q.getAltJ . runIdentity . Q.getRow
      <$> Q.withQE
        defaultTxErrorHandler
        [Q.sql|
    SELECT coalesce(json_agg(typname), '[]')
    FROM pg_catalog.pg_type where typtype = 'b'
   |]
        ()
        True

-- | Clean source database after dropping in metadata
postDropSourceHook ::
  (MonadIO m, MonadError QErr m, MonadBaseControl IO m) =>
  PGSourceConfig ->
  m ()
postDropSourceHook sourceConfig = do
  -- Clean traces of Hasura in source database
  --
  -- There are three type of database we have to consider here, which we
  -- refer to as types 1, 2, and 3 below:
  --   1. default postgres source (no separate metadata database)
  --   In this case, we want to drop nothing.
  --
  --   2. dedicated metadata database
  --   In this case, we want to only drop source-related tables ("event_log",
  --   "hdb_source_catalog_version", etc), leaving the rest of the schema intact.
  --
  --   3. non-default postgres source (necessarily without metadata tables)
  --   In this case, we want to drop the entire "hdb_catalog" schema.
  liftEitherM $
    runPgSourceWriteTx sourceConfig $ do
      hdbMetadataTableExist <- doesTableExist "hdb_catalog" "hdb_metadata"
      eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
      if
          -- If "hdb_metadata" and "event_log" tables are found in the "hdb_catalog" schema,
          -- then this implies the source is being used as the default postgres source, i.e.
          -- this is a default postgres source (type 1 above).
          -- In this case we don't drop anything in the catalog schema.
          | hdbMetadataTableExist && eventLogTableExist -> pure ()
          -- However, it is possible that the above condition is not met for a default
          -- postgres source. This will happen if no event triggers have been defined,
          -- because we initialise event catalog tables only when required (i.e. when
          -- a trigger is defined).
          --
          -- This could lead to a possible problem where "hdb_metadata" exists, "event_log"
          -- does not exist, but the _other_ source-related tables exist. In that case, we
          -- would end up dropping them here, which would go against our requirements above.
          -- However, observe that these tables are always all created or destroyed together,
          -- in single transactions where we run setup/teardown SQL files, so this condition
          -- is guaranteed to not take place.
          --
          -- So if only "hdb_metadata" exists, we have one of two possible cases:
          --   * this is a metadata database (type 2) and we can drop all source-related tables
          --   * this is a default database (type 1) which has no source-related tables (because
          --     it has no "event_log" table, it cannot have the others, because of the previous
          --     argument)
          --
          -- It should be clear that we can now safely issue DROP IF EXISTS statements for
          -- all source-related tables now according to the spec above. The IF EXISTS lets us
          -- handle both cases uniformly, doing nothing in the second case, and for metadata
          -- databases, we drop only source-related tables from the database's "hdb_catalog" schema.
          | hdbMetadataTableExist ->
            Q.multiQE
              defaultTxErrorHandler
              $(makeRelativeToProject "src-rsr/drop_pg_source.sql" >>= Q.sqlFromFile)
          -- Otherwise, we have a non-default postgres source, which has no metadata tables.
          -- We drop the entire "hdb_catalog" schema as discussed above.
          | otherwise -> dropHdbCatalogSchema

  -- Destory postgres source connection
  liftIO $ _pecDestroyConn $ _pscExecCtx sourceConfig

  -- Run other drop hooks configured at source creation time
  liftIO $ _pscPostDropHook sourceConfig
