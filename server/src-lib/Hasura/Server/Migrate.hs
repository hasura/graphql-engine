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
  , getMigratedFrom
  , migrateCatalog
  , latestCatalogVersion
  , downgradeCatalog
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                          as A
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import qualified Database.PG.Query                   as Q
import qualified Language.Haskell.TH.Lib             as TH
import qualified Language.Haskell.TH.Syntax          as TH

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Time.Clock                     (UTCTime)
import           System.Directory                    (doesFileExist)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Logging                      (Hasura, LogLevel (..), ToEngineLog (..))
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DDL.Schema.LegacyCatalog
import           Hasura.RQL.Types
import           Hasura.Server.Init                  (DowngradeOptions (..), databaseUrlEnv)
import           Hasura.Server.Logging               (StartupLog (..))
import           Hasura.Server.Migrate.Version       (latestCatalogVersion,
                                                      latestCatalogVersionString)

data MigrationResult
  = MRNothingToDo
  | MRInitialized
  | MRMigrated Text -- ^ old catalog version
  deriving (Show, Eq)

instance ToEngineLog MigrationResult Hasura where
  toEngineLog result = toEngineLog $ StartupLog
    { slLogLevel = LevelInfo
    , slKind = "catalog_migrate"
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

getMigratedFrom
  :: MigrationResult
  -> Maybe Float -- ^ We have version 0.8 as non integral catalog version
getMigratedFrom = \case
  MRNothingToDo -> Nothing
  MRInitialized -> Nothing
  MRMigrated t  -> readMaybe (T.unpack t)

-- A migration and (hopefully) also its inverse if we have it.
-- Polymorphic because `m` can be any `MonadTx`, `MonadIO` when
-- used in the `migrations` function below.
data MigrationPair m = MigrationPair
  { mpMigrate :: m ()
  , mpDown    :: Maybe (m ())
  }

migrateCatalog
  :: forall m
   . ( MonadTx m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => Maybe SourceConfiguration
  -> UTCTime
  -> m (MigrationResult, Metadata)
migrateCatalog maybeDefaultSourceConfig migrationTime = do
  migrationResult <- doesSchemaExist (SchemaName "hdb_catalog") >>= \case
    False -> initialize True
    True  -> doesTableExist (SchemaName "hdb_catalog") (TableName "hdb_version") >>= \case
      False -> initialize False
      True  -> migrateFrom =<< getCatalogVersion
  metadata <- liftTx fetchMetadataFromCatalog
  pure (migrationResult, metadata)
  where
    -- initializes the catalog, creating the schema if necessary
    initialize :: Bool -> m MigrationResult
    initialize createSchema =  do
      liftTx $ Q.catchE defaultTxErrorHandler $
        when createSchema $ Q.unitQ "CREATE SCHEMA hdb_catalog" () False
      enablePgcryptoExtension
      runTx $(Q.sqlFromFile "src-rsr/initialise.sql")
      updateCatalogVersion

      let emptyMetadata' = case maybeDefaultSourceConfig of
            Nothing -> emptyMetadata
            Just defaultSourceConfig ->
              -- insert metadata with default source
              let defaultSourceMetadata =
                    SourceMetadata defaultSource mempty mempty defaultSourceConfig
                  sources = OMap.singleton defaultSource defaultSourceMetadata
              in emptyMetadata{_metaSources = sources}

      liftTx $ setMetadataInCatalog emptyMetadata'
      pure MRInitialized

    -- migrates an existing catalog to the latest version from an existing verion
    migrateFrom :: Text -> m MigrationResult
    migrateFrom previousVersion
      | previousVersion == latestCatalogVersionString = pure MRNothingToDo
      | [] <- neededMigrations =
          throw400 NotSupported $
            "Cannot use database previously used with a newer version of graphql-engine (expected"
              <> " a catalog version <=" <> latestCatalogVersionString <> ", but the current version"
              <> " is " <> previousVersion <> ")."
      | otherwise = do
          traverse_ (mpMigrate . snd) neededMigrations
          updateCatalogVersion
          pure $ MRMigrated previousVersion
      where
        neededMigrations =
          dropWhile ((/= previousVersion) . fst) (migrations maybeDefaultSourceConfig False)

    updateCatalogVersion = setCatalogVersion latestCatalogVersionString migrationTime

downgradeCatalog
  :: forall m. (MonadIO m, MonadTx m)
  => Maybe SourceConfiguration -> DowngradeOptions -> UTCTime -> m MigrationResult
downgradeCatalog defaultSourceConfig opts time = do
    downgradeFrom =<< getCatalogVersion
  where
    -- downgrades an existing catalog to the specified version
    downgradeFrom :: Text -> m MigrationResult
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
            (reverse (migrations defaultSourceConfig (dgoDryRun opts)))

        downgrade
          :: Text
          -> Text
          -> [(Text, MigrationPair m)]
          -> Either Text [m ()]
        downgrade lower upper = skipFutureDowngrades where
          -- We find the list of downgrade scripts to run by first
          -- dropping any downgrades which correspond to newer versions
          -- of the schema than the one we're running currently.
          -- Then we take migrations as needed until we reach the target
          -- version, dropping any remaining migrations from the end of the
          -- (reversed) list.
          skipFutureDowngrades, dropOlderDowngrades :: [(Text, MigrationPair m)] -> Either Text [m ()]
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

migrations
  :: forall m. (MonadIO m, MonadTx m)
  => Maybe SourceConfiguration -> Bool -> [(Text, MigrationPair m)]
migrations maybeDefaultSourceConfig dryRun =
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
            in [| ( $(TH.lift $ tshow from)
                  , MigrationPair
                      $(migrationFromFile (show from) (show to))
                      $(migrationFromFileMaybe (show to) (show from))
                  ) |]
      in TH.listE
        -- version 0.8 is the only non-integral catalog version
        $  [| ("0.8", MigrationPair $(migrationFromFile "08" "1") Nothing) |]
        :  migrationsFromFile [2..3]
        ++ [| ("3", MigrationPair from3To4 Nothing) |]
        :  migrationsFromFile [5..42]
        ++ [[| ("42", MigrationPair from42To43 (Just from43To42)) |]]
     )
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

    from42To43 = do
      let query = $(Q.sqlFromFile "src-rsr/migrations/42_to_43.sql")
      if dryRun then (liftIO . TIO.putStrLn . Q.getQueryText) query
        else do
        metadataV2 <- fetchMetadataFromHdbTables
        runTx query
        defaultSourceConfig <- onNothing maybeDefaultSourceConfig $ throw400 NotSupported $
          "cannot migrate to catalog version 43 without --database-url or env var " <> tshow (fst databaseUrlEnv)
        let metadataV3 =
              let MetadataNoSources{..} = metadataV2
                  defaultSourceMetadata =
                    SourceMetadata defaultSource _mnsTables _mnsFunctions defaultSourceConfig
              in Metadata (OMap.singleton defaultSource defaultSourceMetadata)
                   _mnsRemoteSchemas _mnsQueryCollections _mnsAllowlist _mnsCustomTypes _mnsActions _mnsCronTriggers
        liftTx $ setMetadataInCatalog metadataV3

    from43To42 = do
      let query = $(Q.sqlFromFile "src-rsr/migrations/43_to_42.sql")
      if dryRun then (liftIO . TIO.putStrLn . Q.getQueryText) query
        else do
        Metadata{..} <- liftTx fetchMetadataFromCatalog
        runTx query
        metadataV2 <- case OMap.toList _metaSources of
          [] -> pure $ MetadataNoSources mempty mempty mempty mempty mempty emptyCustomTypes mempty mempty
          [(_, SourceMetadata{..})] ->
            pure $ MetadataNoSources _smTables _smFunctions _metaRemoteSchemas _metaQueryCollections
                      _metaAllowlist _metaCustomTypes _metaActions _metaCronTriggers
          _ -> throw400 NotSupported "Cannot downgrade since there are more than one source"
        liftTx $ runHasSystemDefinedT (SystemDefined False) $ saveMetadataToHdbTables metadataV2
        recreateSystemMetadata


runTx :: (MonadTx m) => Q.Query -> m ()
runTx = liftTx . Q.multiQE defaultTxErrorHandler
