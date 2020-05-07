-- | Migrations for the Hasura catalog.
--
-- To add a new migration:
--
--   1. Bump the catalog version number in @src-rsr/catalog_version.txt@.
--   2. Add a migration script in the @src-rsr/migrations/@ directory with the name
--      @<old version>_to_<new version>.sql@.
--   3. Create a downgrade script in the @src-rsr/migrations/@ directory with the name
--      @<new version>_to_<old version>.sql@.
--   4. If making a new release, add the mapping from application version to catalog
--      schema version in @src-rsr/catalog_versions.txt@.
--   5. If appropriate, add the change to @server/src-rsr/initialise.sql@ for fresh installations
--      of hasura.
--
-- The Template Haskell code in this module will automatically compile the new migration script into
-- the @graphql-engine@ executable.
module Hasura.Server.Migrate
  ( MigrationResult(..)
  , migrateCatalog
  , latestCatalogVersion
  , recreateSystemMetadata
  , dropCatalog
  , downgradeCatalog
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as A
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Database.PG.Query             as Q
import qualified Database.PG.Query.Connection  as Q
import qualified Language.Haskell.TH.Lib       as TH
import qualified Language.Haskell.TH.Syntax    as TH

import           Control.Lens                  (view, _2)
import           Control.Monad.Unique
import           Data.Time.Clock               (UTCTime)

import           Hasura.Logging                (Hasura, LogLevel (..), ToEngineLog (..))
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Schema
import           Hasura.Server.Init            (DowngradeOptions (..))
import           Hasura.RQL.Types
import           Hasura.Server.Logging         (StartupLog (..))
import           Hasura.Server.Migrate.Version (latestCatalogVersion, latestCatalogVersionString)
import           Hasura.Server.Version         (HasVersion)
import           Hasura.SQL.Types
import           System.Directory              (doesFileExist)

dropCatalog :: (MonadTx m) => m ()
dropCatalog = liftTx $ Q.catchE defaultTxErrorHandler $ do
  -- This is where the generated views and triggers are stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_catalog CASCADE" () False

data MigrationResult
  = MRNothingToDo
  | MRInitialized
  | MRMigrated T.Text -- ^ old catalog version
  deriving (Show, Eq)

instance ToEngineLog MigrationResult Hasura where
  toEngineLog result = toEngineLog $ StartupLog
    { slLogLevel = LevelInfo
    , slKind = "db_migrate"
    , slInfo = A.toJSON $ case result of
        MRNothingToDo ->
          "Already at the latest catalog version (" <> latestCatalogVersionString
            <> "); nothing to do."
        MRInitialized ->
          "Successfully initialized the catalog (at version " <> latestCatalogVersionString <> ")."
        MRMigrated oldVersion ->
          "Successfully migrated from catalog version " <> oldVersion <> " to version "
            <> latestCatalogVersionString <> "."
    }

-- A migration and (hopefully) also its inverse if we have it.
-- Polymorphic because `m` can be any `MonadTx`, `MonadIO` when
-- used in the `migrations` function below.
data MigrationPair m = MigrationPair
  { mpMigrate :: m ()
  , mpDown :: Maybe (m ())
  }

migrateCatalog
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadTx m
     , MonadUnique m
     , HasHttpManager m
     , HasSQLGenCtx m
     )
  => UTCTime
  -> m (MigrationResult, RebuildableSchemaCache m)
migrateCatalog migrationTime = do
  doesSchemaExist (SchemaName "hdb_catalog") >>= \case
    False -> initialize True
    True  -> doesTableExist (SchemaName "hdb_catalog") (TableName "hdb_version") >>= \case
      False -> initialize False
      True  -> migrateFrom =<< getCatalogVersion
  where
    -- initializes the catalog, creating the schema if necessary
    initialize :: Bool -> m (MigrationResult, RebuildableSchemaCache m)
    initialize createSchema =  do
      liftTx $ Q.catchE defaultTxErrorHandler $
        when createSchema $ do
          Q.unitQ "CREATE SCHEMA hdb_catalog" () False
          -- This is where the generated views and triggers are stored
          Q.unitQ "CREATE SCHEMA hdb_views" () False

      isExtensionAvailable (SchemaName "pgcrypto") >>= \case
        -- only if we created the schema, create the extension
        True -> when createSchema $ liftTx $ Q.unitQE needsPGCryptoError
          "CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA public" () False
        False -> throw500 $
          "pgcrypto extension is required, but could not find the extension in the "
          <> "PostgreSQL server. Please make sure this extension is available."

      runTx $(Q.sqlFromFile "src-rsr/initialise.sql")
      schemaCache <- buildCacheAndRecreateSystemMetadata
      updateCatalogVersion
      pure (MRInitialized, schemaCache)
      where
        needsPGCryptoError e@(Q.PGTxErr _ _ _ err) =
          case err of
            Q.PGIUnexpected _ -> requiredError
            Q.PGIStatement pgErr -> case Q.edStatusCode pgErr of
              Just "42501" -> err500 PostgresError permissionsMessage
              _            -> requiredError
          where
            requiredError =
              (err500 PostgresError requiredMessage) { qeInternal = Just $ A.toJSON e }
            requiredMessage =
              "pgcrypto extension is required, but it could not be created;"
              <> " encountered unknown postgres error"
            permissionsMessage =
              "pgcrypto extension is required, but the current user doesn’t have permission to"
              <> " create it. Please grant superuser permission, or setup the initial schema via"
              <> " https://hasura.io/docs/1.0/graphql/manual/deployment/postgres-permissions.html"

    -- migrates an existing catalog to the latest version from an existing verion
    migrateFrom :: T.Text -> m (MigrationResult, RebuildableSchemaCache m)
    migrateFrom previousVersion
      | previousVersion == latestCatalogVersionString = do
          schemaCache <- buildRebuildableSchemaCache
          pure (MRNothingToDo, schemaCache)
      | [] <- neededMigrations =
          throw400 NotSupported $
            "Cannot use database previously used with a newer version of graphql-engine (expected"
              <> " a catalog version <=" <> latestCatalogVersionString <> ", but the current version"
              <> " is " <> previousVersion <> ")."
      | otherwise = do
          traverse_ (mpMigrate . snd) neededMigrations
          schemaCache <- buildCacheAndRecreateSystemMetadata
          updateCatalogVersion
          pure (MRMigrated previousVersion, schemaCache)
      where
        neededMigrations = dropWhile ((/= previousVersion) . fst) (migrations False)

    buildCacheAndRecreateSystemMetadata :: m (RebuildableSchemaCache m)
    buildCacheAndRecreateSystemMetadata = do
      schemaCache <- buildRebuildableSchemaCache
      view _2 <$> runCacheRWT schemaCache recreateSystemMetadata

    updateCatalogVersion = setCatalogVersion latestCatalogVersionString migrationTime

    doesSchemaExist schemaName =
      liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
        SELECT EXISTS
        ( SELECT 1 FROM information_schema.schemata
          WHERE schema_name = $1
        ) |] (Identity schemaName) False

    doesTableExist schemaName tableName =
      liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
        SELECT EXISTS
        ( SELECT 1 FROM pg_tables
          WHERE schemaname = $1 AND tablename = $2
        ) |] (schemaName, tableName) False

    isExtensionAvailable schemaName =
      liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
        SELECT EXISTS
        ( SELECT 1 FROM pg_catalog.pg_available_extensions
          WHERE name = $1
        ) |] (Identity schemaName) False

downgradeCatalog :: forall m. (MonadIO m, MonadTx m) => DowngradeOptions -> UTCTime -> m MigrationResult
downgradeCatalog opts time = do
    downgradeFrom =<< getCatalogVersion
  where
    -- downgrades an existing catalog to the specified version
    downgradeFrom :: T.Text -> m MigrationResult
    downgradeFrom previousVersion
        | previousVersion == dgoTargetVersion opts = do
            pure MRNothingToDo
        | otherwise =
            case neededDownMigrations (dgoTargetVersion opts) of
              Left reason ->
                throw400 NotSupported $
                  "This downgrade path (from "
                    <> previousVersion <> " to "
                    <> dgoTargetVersion opts <>
                    ") is not supported, because "
                    <> reason
              Right path -> do
                sequence_ path
                unless (dgoDryRun opts) do
                  setCatalogVersion (dgoTargetVersion opts) time
                pure (MRMigrated previousVersion)

      where
        neededDownMigrations newVersion =
          downgrade previousVersion newVersion
            (reverse (migrations (dgoDryRun opts)))

        downgrade
          :: T.Text
          -> T.Text
          -> [(T.Text, MigrationPair m)]
          -> Either T.Text [m ()]
        downgrade lower upper = skipFutureDowngrades where
          -- We find the list of downgrade scripts to run by first
          -- dropping any downgrades which correspond to newer versions
          -- of the schema than the one we're running currently.
          -- Then we take migrations as needed until we reach the target
          -- version, dropping any remaining migrations from the end of the
          -- (reversed) list.
          skipFutureDowngrades, dropOlderDowngrades :: [(T.Text, MigrationPair m)] -> Either T.Text [m ()]
          skipFutureDowngrades xs | previousVersion == lower = dropOlderDowngrades xs
          skipFutureDowngrades [] = Left "the starting version is unrecognized."
          skipFutureDowngrades ((x, _):xs)
            | x == lower = dropOlderDowngrades xs
            | otherwise = skipFutureDowngrades xs

          dropOlderDowngrades [] = Left "the target version is unrecognized."
          dropOlderDowngrades ((x, MigrationPair{ mpDown = Nothing }):_) =
            Left $ "there is no available migration back to version " <> x <> "."
          dropOlderDowngrades ((x, MigrationPair{ mpDown = Just y }):xs)
            | x == upper = Right [y]
            | otherwise = (y:) <$> dropOlderDowngrades xs

-- | The old 0.8 catalog version is non-integral, so we store it in the database as a
-- string.
getCatalogVersion :: MonadTx m => m Text
getCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  [Q.sql| SELECT version FROM hdb_catalog.hdb_version |] () False

setCatalogVersion :: MonadTx m => Text -> UTCTime -> m ()
setCatalogVersion ver time = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO hdb_catalog.hdb_version (version, upgraded_on) VALUES ($1, $2)
    ON CONFLICT ((version IS NOT NULL))
    DO UPDATE SET version = $1, upgraded_on = $2
  |] (ver, time) False

migrations :: forall m. (MonadIO m, MonadTx m) => Bool -> [(T.Text, MigrationPair m)]
migrations dryRun =
    -- We need to build the list of migrations at compile-time so that we can compile the SQL
    -- directly into the executable using `Q.sqlFromFile`. The GHC stage restriction makes
    -- doing this a little bit awkward (we can’t use any definitions in this module at
    -- compile-time), but putting a `let` inside the splice itself is allowed.
    $(let migrationFromFile from to =
            let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
             in [| runTxOrPrint $(Q.sqlFromFile path) |]
          migrationFromFileMaybe from to = do
            let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
            exists <- TH.runIO (doesFileExist path)
            if exists
              then [| Just (runTxOrPrint $(Q.sqlFromFile path)) |]
              else [| Nothing |]

          migrationsFromFile = map $ \(to :: Integer) ->
            let from = to - 1
            in [| ( $(TH.lift $ T.pack (show from))
                  , MigrationPair
                      $(migrationFromFile (show from) (show to))
                      $(migrationFromFileMaybe (show to) (show from))
                  ) |]
      in TH.listE
        -- version 0.8 is the only non-integral catalog version
        $  [| ("0.8", (MigrationPair $(migrationFromFile "08" "1") Nothing)) |]
        :  migrationsFromFile [2..3]
        ++ [| ("3", (MigrationPair from3To4 Nothing)) |]
        :  migrationsFromFile [5..latestCatalogVersion])
  where
    runTxOrPrint :: Q.Query -> m ()
    runTxOrPrint
      | dryRun =
          liftIO . TIO.putStrLn . Q.getQueryText
      | otherwise = runTx

    from3To4 = liftTx $ Q.catchE defaultTxErrorHandler $ do
      Q.unitQ [Q.sql|
        ALTER TABLE hdb_catalog.event_triggers
        ADD COLUMN configuration JSON |] () False
      eventTriggers <- map uncurryEventTrigger <$> Q.listQ [Q.sql|
        SELECT e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval, e.headers::json
        FROM hdb_catalog.event_triggers e |] () False
      forM_ eventTriggers updateEventTrigger3To4
      Q.unitQ [Q.sql|
        ALTER TABLE hdb_catalog.event_triggers
        DROP COLUMN definition,
        DROP COLUMN query,
        DROP COLUMN webhook,
        DROP COLUMN num_retries,
        DROP COLUMN retry_interval,
        DROP COLUMN headers |] () False
      where
        uncurryEventTrigger (trn, Q.AltJ tDef, w, nr, rint, Q.AltJ headers) =
          EventTriggerConf trn tDef (Just w) Nothing (RetryConf nr rint Nothing) headers
        updateEventTrigger3To4 etc@(EventTriggerConf name _ _ _ _ _) = Q.unitQ [Q.sql|
                                             UPDATE hdb_catalog.event_triggers
                                             SET
                                             configuration = $1
                                             WHERE name = $2
                                             |] (Q.AltJ $ A.toJSON etc, name) True

-- | Drops and recreates all “system-defined” metadata, aka metadata for tables and views in the
-- @information_schema@ and @hdb_catalog@ schemas. These tables and views are tracked to expose them
-- to the console, which allows us to reuse the same functionality we use to implement user-defined
-- APIs to expose the catalog.
--
-- This process has a long and storied history.
--
-- In the past, we reused the same machinery we use for CLI migrations to define our own internal
-- metadata migrations. This caused trouble, however, as we’d have to run those migrations in
-- lockstep with our SQL migrations to ensure the two didn’t get out of sync. This in turn caused
-- trouble because those migrations would hit code paths inside @graphql-engine@ to add or remove
-- things from the @pg_catalog@ tables, and /that/ in turn would fail because we hadn’t finished
-- running the SQL migrations, so we were running a new version of the code against an old version
-- of the schema! That caused #2826.
--
-- To fix that, #2379 switched to the approach of just dropping and recreating all system metadata
-- every time we run any SQL migrations. But /that/ in turn caused trouble due to the way we were
-- constantly rebuilding the schema cache (#3354), causing us to switch to incremental schema cache
-- construction (#3394). However, although that mostly resolved the problem, we still weren’t
-- totally out of the woods, as the incremental construction was still too slow on slow Postgres
-- instances (#3654).
--
-- To sidestep the whole issue, as of #3686 we now just create all the system metadata in code here,
-- and we only rebuild the schema cache once, at the very end. This is a little unsatisfying, since
-- it means our internal migrations are “blessed” compared to user-defined CLI migrations. If we
-- improve CLI migrations further in the future, maybe we can switch back to using that approach,
-- instead.
recreateSystemMetadata :: (MonadTx m, CacheRWM m) => m ()
recreateSystemMetadata = do
  runTx $(Q.sqlFromFile "src-rsr/clear_system_metadata.sql")
  runHasSystemDefinedT (SystemDefined True) $ for_ systemMetadata \(tableName, tableRels) -> do
    saveTableToCatalog tableName False emptyTableConfig
    for_ tableRels \case
      Left relDef -> insertRelationshipToCatalog tableName ObjRel relDef
      Right relDef -> insertRelationshipToCatalog tableName ArrRel relDef
  buildSchemaCacheStrict
  where
    systemMetadata :: [(QualifiedTable, [Either ObjRelDef ArrRelDef])]
    systemMetadata =
      [ table "information_schema" "tables" []
      , table "information_schema" "schemata" []
      , table "information_schema" "views" []
      , table "information_schema" "columns" []
      , table "hdb_catalog" "hdb_table"
        [ objectRel $$(nonEmptyText "detail") $
          manualConfig "information_schema" "tables" tableNameMapping
        , objectRel $$(nonEmptyText "primary_key") $
          manualConfig "hdb_catalog" "hdb_primary_key" tableNameMapping
        , arrayRel $$(nonEmptyText "columns") $
          manualConfig "information_schema" "columns" tableNameMapping
        , arrayRel $$(nonEmptyText "foreign_key_constraints") $
          manualConfig "hdb_catalog" "hdb_foreign_key_constraint" tableNameMapping
        , arrayRel $$(nonEmptyText "relationships") $
          manualConfig "hdb_catalog" "hdb_relationship" tableNameMapping
        , arrayRel $$(nonEmptyText "permissions") $
          manualConfig "hdb_catalog" "hdb_permission_agg" tableNameMapping
        , arrayRel $$(nonEmptyText "computed_fields") $
          manualConfig "hdb_catalog" "hdb_computed_field" tableNameMapping
        , arrayRel $$(nonEmptyText "check_constraints") $
          manualConfig "hdb_catalog" "hdb_check_constraint" tableNameMapping
        , arrayRel $$(nonEmptyText "unique_constraints") $
          manualConfig "hdb_catalog" "hdb_unique_constraint" tableNameMapping
        , arrayRel $$(nonEmptyText "event_triggers") $
          manualConfig "hdb_catalog" "event_triggers"
          [ ("table_schema","schema_name")
          , ("table_name","table_name")]
        ]
      , table "hdb_catalog" "hdb_primary_key" []
      , table "hdb_catalog" "hdb_foreign_key_constraint" []
      , table "hdb_catalog" "hdb_relationship" []
      , table "hdb_catalog" "hdb_permission_agg" []
      , table "hdb_catalog" "hdb_computed_field" []
      , table "hdb_catalog" "hdb_check_constraint" []
      , table "hdb_catalog" "hdb_unique_constraint" []
      , table "hdb_catalog" "event_triggers"
        [ arrayRel $$(nonEmptyText "events") $
          manualConfig "hdb_catalog" "event_log" [("name", "trigger_name")] ]
      , table "hdb_catalog" "event_log"
        [ objectRel $$(nonEmptyText "trigger") $
          manualConfig "hdb_catalog" "event_triggers" [("trigger_name", "name")]
        , arrayRel $$(nonEmptyText "logs") $ RUFKeyOn $
          ArrRelUsingFKeyOn (QualifiedObject "hdb_catalog" "event_invocation_logs") "event_id" ]
      , table "hdb_catalog" "event_invocation_logs"
        [ objectRel $$(nonEmptyText "event") $ RUFKeyOn "event_id" ]
      , table "hdb_catalog" "hdb_function" []
      , table "hdb_catalog" "hdb_function_agg"
        [ objectRel $$(nonEmptyText "return_table_info") $ manualConfig "hdb_catalog" "hdb_table"
          [ ("return_type_schema", "table_schema")
          , ("return_type_name", "table_name") ] ]
      , table "hdb_catalog" "remote_schemas" []
      , table "hdb_catalog" "hdb_version" []
      , table "hdb_catalog" "hdb_query_collection" []
      , table "hdb_catalog" "hdb_allowlist" []
      , table "hdb_catalog" "hdb_custom_types" []
      , table "hdb_catalog" "hdb_action_permission" []
      , table "hdb_catalog" "hdb_action"
        [ arrayRel $$(nonEmptyText "permissions") $ manualConfig "hdb_catalog" "hdb_action_permission"
          [("action_name", "action_name")]
        ]
      , table "hdb_catalog" "hdb_action_log" []
      , table "hdb_catalog" "hdb_role"
        [ arrayRel $$(nonEmptyText "action_permissions") $ manualConfig "hdb_catalog" "hdb_action_permission"
          [("role_name", "role_name")]
        , arrayRel $$(nonEmptyText "permissions") $ manualConfig "hdb_catalog" "hdb_permission_agg"
          [("role_name", "role_name")]
        ]
      ]

    tableNameMapping =
      [ ("table_schema", "table_schema")
      , ("table_name", "table_name") ]

    table schemaName tableName relationships = (QualifiedObject schemaName tableName, relationships)
    objectRel name using = Left $ RelDef (RelName name) using Nothing
    arrayRel name using = Right $ RelDef (RelName name) using Nothing
    manualConfig schemaName tableName columns =
      RUManual $ RelManualConfig (QualifiedObject schemaName tableName) (HM.fromList columns)

runTx :: (MonadTx m) => Q.Query -> m ()
runTx = liftTx . Q.multiQE defaultTxErrorHandler
