{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Postgres DDL Source
--
-- A Source is a connected database. One can have multiple sources of the same
-- kind (e.g. Postgres).
--
-- This module provides ways to fetch, update, and deal with table and function
-- metadata and hdb_catalog migrations for a Postgres Source.
--
--    NOTE: Please have a look at the `server/documentation/migration-guidelines.md`
--       before adding any new migration if you haven't already looked at it.
module Hasura.Backends.Postgres.DDL.Source
  ( ToMetadataFetchQuery,
    FetchTableMetadata (..),
    FetchFunctionMetadata (..),
    prepareCatalog,
    postDropSourceHook,
    resolveDatabaseMetadata,
    resolveSourceConfig,
    logPGSourceCatalogMigrationLockedQueries,
    -- naughty exports, forgive me padre
    pgFetchTableMetadata,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON, toJSON)
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.List.Extended qualified as LE
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.DDL.EventTrigger (dropTriggerQ)
import Hasura.Backends.Postgres.DDL.Source.Version
import Hasura.Backends.Postgres.SQL.Types hiding (FunctionName)
import Hasura.Backends.Postgres.Types.ComputedField
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
import Hasura.RQL.Types.Metadata (SourceMetadata (..), _cfmDefinition)
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Server.Migrate.Internal
import Hasura.Server.Migrate.Version qualified as Version
import Hasura.Table.Cache
import Hasura.Table.Metadata (TableMetadata (..))
import Language.Haskell.TH.Lib qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

-- | We differentiate the handling of metadata between Citus, Cockroach and Vanilla
-- Postgres because Citus imposes limitations on the types of joins that it
-- permits, which then limits the types of relations that we can track.
class ToMetadataFetchQuery (pgKind :: PostgresKind) where
  tableMetadata :: PG.Query

instance ToMetadataFetchQuery 'Vanilla where
  tableMetadata = $(makeRelativeToProject "src-rsr/pg_table_metadata.sql" >>= PG.sqlFromFile)

instance ToMetadataFetchQuery 'Citus where
  tableMetadata = $(makeRelativeToProject "src-rsr/citus_table_metadata.sql" >>= PG.sqlFromFile)

instance ToMetadataFetchQuery 'Cockroach where
  tableMetadata = $(makeRelativeToProject "src-rsr/cockroach_table_metadata.sql" >>= PG.sqlFromFile)

resolveSourceConfig ::
  (MonadIO m, MonadResolveSource m) =>
  SourceName ->
  PostgresConnConfiguration ->
  BackendSourceKind ('Postgres pgKind) ->
  BackendConfig ('Postgres pgKind) ->
  Env.Environment ->
  manager ->
  m (Either QErr (SourceConfig ('Postgres pgKind)))
resolveSourceConfig name config _backendKind _backendConfig env _manager = runExceptT do
  sourceResolver <- getPGSourceResolver
  liftEitherM $ liftIO $ sourceResolver env name config

-- | 'PGSourceLockQuery' is a data type which represents the contents of a single object of the
--   locked queries which are queried from the `pg_stat_activity`. See `logPGSourceCatalogMigrationLockedQueries`.
data PGSourceLockQuery = PGSourceLockQuery
  { _psqaQuery :: Text,
    _psqaLockGranted :: Maybe Bool,
    _psqaLockMode :: Text,
    _psqaTransactionStartTime :: UTCTime,
    _psqaQueryStartTime :: UTCTime,
    _psqaWaitEventType :: Text,
    _psqaBlockingQuery :: Text
  }
  deriving stock (Generic)

instance FromJSON PGSourceLockQuery where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON PGSourceLockQuery where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

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
  (MonadIO m) =>
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
      (PG.getViaJSON . runIdentity . PG.getRow)
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
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
  ( Backend ('Postgres pgKind),
    ToMetadataFetchQuery pgKind,
    FetchFunctionMetadata pgKind,
    FetchTableMetadata pgKind,
    MonadIO m,
    MonadBaseControl IO m,
    FF.HasFeatureFlagChecker m
  ) =>
  SourceMetadata ('Postgres pgKind) ->
  SourceConfig ('Postgres pgKind) ->
  m (Either QErr (DBObjectsIntrospection ('Postgres pgKind)))
resolveDatabaseMetadata sourceMetadata sourceConfig = do
  enableNamingConventionSep2023 <- FF.checkFlag FF.namingConventionSep2023
  runExceptT $ _pecRunTx (_pscExecCtx sourceConfig) (PGExecCtxInfo (Tx PG.ReadOnly Nothing) InternalRawQuery) do
    tablesMeta <- fetchTableMetadata $ HashMap.keysSet $ InsOrdHashMap.toHashMap $ _smTables sourceMetadata
    let allFunctions =
          Set.fromList
            $ InsOrdHashMap.keys (_smFunctions sourceMetadata) -- Tracked functions
            <> concatMap getComputedFieldFunctionsMetadata (InsOrdHashMap.elems $ _smTables sourceMetadata) -- Computed field functions
    functionsMeta <- fetchFunctionMetadata @pgKind allFunctions
    pgScalars <- fetchPgScalars
    let customization = _smCustomization sourceMetadata
        tCase
          | enableNamingConventionSep2023 = fromMaybe HasuraCase $ _scNamingConvention customization
          | otherwise = HasuraCase
        scalarsMap = HashMap.fromList do
          scalar <- Set.toList pgScalars
          name <- afold @(Either QErr) $ mkScalarTypeName tCase scalar
          pure (name, scalar)
    pure $ DBObjectsIntrospection tablesMeta functionsMeta (ScalarMap scalarsMap) mempty
  where
    -- A helper function to list all functions underpinning computed fields from a table metadata
    getComputedFieldFunctionsMetadata :: TableMetadata ('Postgres pgKind) -> [FunctionName ('Postgres pgKind)]
    getComputedFieldFunctionsMetadata =
      map (_cfdFunction . _cfmDefinition) . InsOrdHashMap.elems . _tmComputedFields

-- | Initialise catalog tables for a source, including those required by the event delivery subsystem.
prepareCatalog ::
  (MonadIO m, MonadBaseControl IO m) =>
  SourceConfig ('Postgres pgKind) ->
  ExceptT QErr m (RecreateEventTriggers, Version.SourceCatalogMigrationState)
-- TODO: SHould we prepare the catalog in Serializable isolation mode like the 'prepareCatalog' of MSSQL
prepareCatalog sourceConfig = _pecRunTx (_pscExecCtx sourceConfig) (PGExecCtxInfo (Tx PG.ReadWrite Nothing) InternalRawQuery) do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
  sourceVersionTableExist <- doesTableExist "hdb_catalog" "hdb_source_catalog_version"
  if
    -- Fresh database
    | not hdbCatalogExist -> liftTx do
        PG.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
        enablePgcryptoExtension $ _pscExtensionsSchema sourceConfig
        initPgSourceCatalog
        return (RETDoNothing, Version.SCMSInitialized $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
    -- Only 'hdb_catalog' schema defined
    | not (sourceVersionTableExist || eventLogTableExist) -> do
        liftTx initPgSourceCatalog
        return (RETDoNothing, Version.SCMSInitialized $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
    -- Source is initialised by pre multisource support servers
    | not sourceVersionTableExist && eventLogTableExist -> do
        -- Update the Source Catalog to v43 to include the new migration
        -- changes. Skipping this step will result in errors.
        currMetadataCatalogVersion <- liftTx getCatalogVersion
        -- we migrate to the 43 version, which is the migration where
        -- metadata separation is introduced
        migrateTo43MetadataCatalog currMetadataCatalogVersion
        liftTx createVersionTable
        -- Migrate the catalog from initial version i.e '0'
        migrateSourceCatalogFrom initialSourceCatalogVersion
    | otherwise -> migrateSourceCatalog
  where
    initPgSourceCatalog = do
      () <- PG.multiQE defaultTxErrorHandler $(makeRelativeToProject "src-rsr/init_pg_source.sql" >>= PG.sqlFromFile)
      setSourceCatalogVersion

    createVersionTable = do
      () <-
        PG.multiQE
          defaultTxErrorHandler
          [PG.sql|
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
          migrationTime <- liftIO getCurrentTime
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

migrateSourceCatalog :: (MonadTx m) => m (RecreateEventTriggers, Version.SourceCatalogMigrationState)
migrateSourceCatalog =
  getSourceCatalogVersion >>= migrateSourceCatalogFrom

-- | `migrateSourceCatalogFrom` migrates the catalog from a lower to a higher version.
--    When there are any changes in the source catalog, then re-create the existing event
--    triggers in the metadata. This is done so that the event triggers be compatible with the
--    changes introduced in the newly added source catalog migrations. When the source is already
--    in the latest catalog version, we do nothing because nothing has changed w.r.t the source catalog
--    so recreating the event triggers will only be extraneous.
migrateSourceCatalogFrom ::
  (MonadTx m) =>
  SourceCatalogVersion pgKind ->
  m (RecreateEventTriggers, Version.SourceCatalogMigrationState)
migrateSourceCatalogFrom prevVersion
  | prevVersion == latestSourceCatalogVersion = pure (RETDoNothing, Version.SCMSNothingToDo $ Version.unSourceCatalogVersion latestSourceCatalogVersion)
  | [] <- neededMigrations =
      throw400 NotSupported
        $ "Expected source catalog version <= "
        <> tshow latestSourceCatalogVersion
        <> ", but the current version is "
        <> tshow prevVersion
  | otherwise = do
      liftTx $ traverse_ snd neededMigrations
      setSourceCatalogVersion
      pure
        ( RETRecreate,
          Version.SCMSMigratedTo
            (Version.unSourceCatalogVersion prevVersion)
            (Version.unSourceCatalogVersion latestSourceCatalogVersion)
        )
  where
    neededMigrations =
      dropWhile ((/= prevVersion) . fst) sourceMigrations

sourceMigrations :: [(SourceCatalogVersion pgKind, PG.TxE QErr ())]
sourceMigrations =
  $( let migrationFromFile from =
           let to = succ from
               path = "src-rsr/pg_source_migrations/" <> show from <> "_to_" <> show to <> ".sql"
            in [|PG.multiQE defaultTxErrorHandler $(makeRelativeToProject path >>= PG.sqlFromFile)|]

         migrationsFromFile = map $ \from ->
           [|($(TH.lift from), $(migrationFromFile from))|]
      in TH.listE $ migrationsFromFile previousSourceCatalogVersions
   )

-- Upgrade the hdb_catalog schema to v43 (Metadata catalog)
upMigrationsUntil43 :: [(Version.MetadataCatalogVersion, PG.TxE QErr ())]
upMigrationsUntil43 =
  $( let migrationFromFile from to =
           let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
            in [|PG.multiQE defaultTxErrorHandler $(makeRelativeToProject path >>= PG.sqlFromFile)|]

         migrationsFromFile = map $ \(to :: Int) ->
           let from = pred to
            in [|
                 ( $(TH.lift (Version.MetadataCatalogVersion from)),
                   $(migrationFromFile (show from) (show to))
                 )
                 |]
      in TH.listE
           -- version 0.8 is the only non-integral catalog version
           -- The 41st migration which included only source catalog migration
           -- was introduced before metadata separation changes were introduced
           -- in the graphql-engine. Now the earlier 41st migration has been
           -- moved to source catalog migrations and the 41st up migration is removed
           -- entirely.
           $ [|(Version.MetadataCatalogVersion08, $(migrationFromFile "08" "1"))|]
           : migrationsFromFile [2 .. 3]
             ++ [|(Version.MetadataCatalogVersion 3, from3To4)|]
           : migrationsFromFile [5 .. 40]
             ++ migrationsFromFile [42 .. 43]
   )

-- | We differentiate for CockroachDB and other PG implementations
-- as our CockroachDB table fetching SQL does not require table information,
-- and fails if it receives unused prepared arguments
-- this distinction should no longer be necessary if this issue is resolved:
-- https://github.com/cockroachdb/cockroach/issues/86375
class FetchTableMetadata (pgKind :: PostgresKind) where
  fetchTableMetadata ::
    forall m.
    ( Backend ('Postgres pgKind),
      ToMetadataFetchQuery pgKind,
      MonadTx m
    ) =>
    Set.HashSet QualifiedTable ->
    m (DBTablesMetadata ('Postgres pgKind))

instance FetchTableMetadata 'Vanilla where
  fetchTableMetadata = pgFetchTableMetadata

instance FetchTableMetadata 'Citus where
  fetchTableMetadata = pgFetchTableMetadata

instance FetchTableMetadata 'Cockroach where
  fetchTableMetadata = cockroachFetchTableMetadata

-- | Fetch Postgres metadata of all user tables
pgFetchTableMetadata ::
  forall pgKind m.
  (Backend ('Postgres pgKind), ToMetadataFetchQuery pgKind, MonadTx m) =>
  Set.HashSet QualifiedTable ->
  m (DBTablesMetadata ('Postgres pgKind))
pgFetchTableMetadata tables = do
  results <-
    liftTx
      $ PG.withQE
        defaultTxErrorHandler
        (tableMetadata @pgKind)
        [PG.ViaJSON $ LE.uniques $ Set.toList tables]
        True
  pure
    $ HashMap.fromList
    $ flip map results
    $ \(schema, table, PG.ViaJSON info) -> (QualifiedObject schema table, info)

-- | Fetch Cockroach metadata of all user tables
cockroachFetchTableMetadata ::
  forall pgKind m.
  (Backend ('Postgres pgKind), ToMetadataFetchQuery pgKind, MonadTx m) =>
  Set.HashSet QualifiedTable ->
  m (DBTablesMetadata ('Postgres pgKind))
cockroachFetchTableMetadata _tables = do
  results <-
    liftTx
      $ PG.rawQE
        defaultTxErrorHandler
        (tableMetadata @pgKind)
        []
        True
  pure
    $ HashMap.fromList
    $ flip map results
    $ \(schema, table, PG.ViaJSON info) -> (QualifiedObject schema table, info)

class FetchFunctionMetadata (pgKind :: PostgresKind) where
  fetchFunctionMetadata ::
    (MonadTx m) =>
    Set.HashSet QualifiedFunction ->
    m (DBFunctionsMetadata ('Postgres pgKind))

instance FetchFunctionMetadata 'Vanilla where
  fetchFunctionMetadata = pgFetchFunctionMetadata

instance FetchFunctionMetadata 'Citus where
  fetchFunctionMetadata = pgFetchFunctionMetadata

instance FetchFunctionMetadata 'Cockroach where
  fetchFunctionMetadata _ = pure mempty

-- | Fetch Postgres metadata for all user functions
pgFetchFunctionMetadata :: (MonadTx m) => Set.HashSet QualifiedFunction -> m (DBFunctionsMetadata ('Postgres pgKind))
pgFetchFunctionMetadata functions = do
  results <-
    liftTx
      $ PG.withQE
        defaultTxErrorHandler
        $(makeRelativeToProject "src-rsr/pg_function_metadata.sql" >>= PG.sqlFromFile)
        [PG.ViaJSON functions]
        True
  pure
    $ HashMap.fromList
    $ flip map results
    $ \(schema, table, PG.ViaJSON infos) -> (QualifiedObject schema table, infos)

-- | Fetch all scalar types from Postgres
fetchPgScalars :: (MonadTx m) => m (HashSet PGScalarType)
fetchPgScalars =
  liftTx
    $ PG.getViaJSON
    . runIdentity
    . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    SELECT coalesce(json_agg(typname), '[]')
    FROM pg_catalog.pg_type where typtype = 'b'
   |]
      ()
      True

-- | Clean source database after dropping in metadata
postDropSourceHook ::
  (MonadIO m, MonadError QErr m, MonadBaseControl IO m) =>
  SourceConfig ('Postgres pgKind) ->
  TableEventTriggers ('Postgres pgKind) ->
  m ()
postDropSourceHook sourceConfig tableTriggersMap = do
  -- Clean traces of Hasura in source database
  --
  -- There are three type of database we have to consider here, which we
  -- refer to as types 1, 2, and 3 below:
  --   1. default postgres source (no separate metadata database)
  --   In this case, we want to only drop source-related tables ("event_log",
  --   "hdb_source_catalog_version", etc), leaving the rest of the schema
  --   intact.
  --
  --   2. dedicated metadata database
  --   Ideally a dedicated metadata database won't have any source related
  --   tables. But if it does, then drop only source-related tables, leaving the
  --   rest of schema intact.
  --
  --   3. non-default postgres source (necessarily without metadata tables)
  --   In this case, we want to drop the entire "hdb_catalog" schema.
  liftEitherM
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ do
      hdbMetadataTableExist <- doesTableExist "hdb_catalog" "hdb_metadata"
      if
        -- If "hdb_metadata" exists, we have one of two possible cases:
        --   * this is a metadata database (type 2)
        --   * this is a default database (type 1)
        --
        -- Both of the possible cases might have source-related tables. And in
        -- both the cases we only want to drop the source-related tables
        -- leaving rest of the schema intact.
        --
        -- To adhere to the spec described above, we use DROP IF EXISTS
        -- statements for all source-related tables. The IF EXISTS lets us
        -- handle both cases uniformly, doing "ideally" nothing in the type 2
        -- database, and for default databases, we drop only source-related
        -- tables from the database's "hdb_catalog" schema.
        | hdbMetadataTableExist -> do
            -- drop the event trigger functions from the table for default sources
            for_ (HashMap.toList tableTriggersMap) $ \(_table, triggers) ->
              for_ triggers $ \triggerName ->
                liftTx $ dropTriggerQ triggerName
            PG.multiQE
              defaultTxErrorHandler
              $(makeRelativeToProject "src-rsr/drop_pg_source.sql" >>= PG.sqlFromFile)
        -- Otherwise, we have a non-default postgres source, which has no metadata tables.
        -- We drop the entire "hdb_catalog" schema as discussed above.
        | otherwise ->
            dropHdbCatalogSchema

  -- Destory postgres source connection
  liftIO $ _pecDestroyConnections (_pscExecCtx sourceConfig)

  -- Run other drop hooks configured at source creation time
  liftIO $ _pscPostDropHook sourceConfig
