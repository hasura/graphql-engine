-- | Migrations for the Hasura catalog.
--
-- To add a new migration:
--
--   1. Bump the catalog version number in "Hasura.Server.Migrate.Version".
--   2. Add a migration script in the @src-rsr/migrations/@ directory with the name
--      @<old version>_to_<new version>.sql@.
--
-- The Template Haskell code in this module will automatically compile the new migration script into
-- the @graphql-engine@ executable.
module Hasura.Server.Migrate
  ( MigrationResult(..)
  , migrateCatalog
  , latestCatalogVersion
  , recreateSystemMetadata
  , dropCatalog
  ) where

import           Data.Time.Clock               (UTCTime)

import           Hasura.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Text                     as T
import qualified Data.Yaml.TH                  as Y
import qualified Database.PG.Query             as Q
import qualified Database.PG.Query.Connection  as Q
import qualified Language.Haskell.TH.Lib       as TH
import qualified Language.Haskell.TH.Syntax    as TH

import           Hasura.Logging                (LogLevel (..), ToEngineLog (..))
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Logging         (StartupLog (..))
import           Hasura.Server.Migrate.Version (latestCatalogVersion,
                                                latestCatalogVersionString)
import           Hasura.Server.Query
import           Hasura.SQL.Types

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

instance ToEngineLog MigrationResult where
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

migrateCatalog
  :: forall m
   . ( MonadTx m
     , HasHttpManager m
     , CacheRWM m
     , UserInfoM m
     , MonadIO m
     , HasSQLGenCtx m
     )
  => UTCTime -> m MigrationResult
migrateCatalog migrationTime = do
  doesSchemaExist (SchemaName "hdb_catalog") >>= \case
    False -> initialize True
    True  -> doesTableExist (SchemaName "hdb_catalog") (TableName "hdb_version") >>= \case
      False -> initialize False
      True  -> migrateFrom =<< getCatalogVersion
  where
    -- initializes the catalog, creating the schema if necessary
    initialize :: Bool -> m MigrationResult
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
      recreateSystemMetadata
      updateCatalogVersion
      pure MRInitialized
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
              <> " https://docs.hasura.io/1.0/graphql/manual/deployment/postgres-permissions.html"

    -- migrates an existing catalog to the latest version from an existing verion
    migrateFrom :: T.Text -> m MigrationResult
    migrateFrom previousVersion
      | previousVersion == latestCatalogVersionString = pure MRNothingToDo
      | [] <- neededMigrations = throw400 NotSupported $
          "Cannot use database previously used with a newer version of graphql-engine (expected"
            <> " a catalog version <=" <> latestCatalogVersionString <> ", but the current version"
            <> " is " <> previousVersion <> ")."
      | otherwise =
          traverse_ snd neededMigrations *> postMigrate $> MRMigrated previousVersion
      where
        neededMigrations = dropWhile ((/= previousVersion) . fst) migrations
        postMigrate = updateCatalogVersion *> recreateSystemMetadata *> buildSchemaCacheStrict

        migrations :: [(T.Text, m ())]
        migrations =
          -- We need to build the list of migrations at compile-time so that we can compile the SQL
          -- directly into the executable using `Q.sqlFromFile`. The GHC stage restriction makes
          -- doing this a little bit awkward (we can’t use any definitions in this module at
          -- compile-time), but putting a `let` inside the splice itself is allowed.
          $(let migrationFromFile from to =
                  let path = "src-rsr/migrations/" <> from <> "_to_" <> to <> ".sql"
                  in [| runTx $(Q.sqlFromFile path) |]
                migrationsFromFile = map $ \(to :: Integer) ->
                  let from = to - 1
                  in [| ( $(TH.lift $ T.pack (show from))
                        , $(migrationFromFile (show from) (show to))
                        ) |]
            in TH.listE
              -- version 0.8 is the only non-integral catalog version
              $  [| ("0.8", $(migrationFromFile "08" "1")) |]
              :  migrationsFromFile [2..3]
              ++ [| ("3", from3To4) |]
              :  migrationsFromFile [5..latestCatalogVersion])

    -- the old 0.8 catalog version is non-integral, so we store it in the database as a string
    getCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
      [Q.sql| SELECT version FROM hdb_catalog.hdb_version |] () False

    updateCatalogVersion = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        INSERT INTO hdb_catalog.hdb_version (version, upgraded_on) VALUES ($1, $2)
        ON CONFLICT ((version IS NOT NULL))
        DO UPDATE SET version = $1, upgraded_on = $2
      |] (latestCatalogVersionString, migrationTime) False

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

recreateSystemMetadata
  :: ( MonadTx m
     , HasHttpManager m
     , CacheRWM m
     , UserInfoM m
     , MonadIO m
     , HasSQLGenCtx m
     )
  => m ()
recreateSystemMetadata = do
  runTx $(Q.sqlFromFile "src-rsr/clear_system_metadata.sql")
  void . runHasSystemDefinedT (SystemDefined True) $
    runQueryM $$(Y.decodeFile "src-rsr/hdb_metadata.yaml")

runTx :: (MonadTx m) => Q.Query -> m ()
runTx = liftTx . Q.multiQE defaultTxErrorHandler
