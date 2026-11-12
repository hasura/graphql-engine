{-# LANGUAGE QuasiQuotes #-}

module Test.Hasura.Server.SourceCatalogSuite (suite) where

import Control.Exception (bracket_)
import Data.Text qualified as T
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.DDL.Source qualified as Source
import Hasura.Backends.Postgres.DDL.Source.Version qualified as SourceVersion
import Hasura.Backends.Postgres.Instances.Transport ()
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (..))
import Hasura.Server.Migrate.Version qualified as Version
import Test.Hspec

-- These tests use the same disposable database as the other test-postgres
-- suites. A FOR ALL TABLES publication includes tables created after it, which
-- is important for reproducing the very first version upsert during bootstrap.
suite :: PGExecCtx -> PG.ConnInfo -> Spec
suite pgExecCtx pgConnInfo =
  beforeAll makeSourceConfig
    $ aroundWith (\test sourceConfig -> bracket_ (resetCatalog sourceConfig) (resetCatalog sourceConfig) (test sourceConfig))
    $ do
      for_ [False, True] \schemaExists ->
        it ("initializes under an UPDATE publication with hdb_catalog " <> if schemaExists then "present" else "absent") \sourceConfig -> do
          when schemaExists
            $ runSQL sourceConfig
            $ PG.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
          createPublication sourceConfig
          (recreate, migrationState) <- prepareCatalog sourceConfig
          recreate `shouldBe` RETDoNothing
          case migrationState of
            Version.SCMSInitialized 5 -> pure ()
            _ -> expectationFailure "Expected source catalog initialization at version 5"
          assertVersionAndIdentity sourceConfig
          -- Exercise the conflicting-row branch too, without changing the upsert.
          runSQL sourceConfig SourceVersion.setSourceCatalogVersion
          assertVersionAndIdentity sourceConfig
          assertNoMigration sourceConfig

      it "migrates a version 4 catalog before stamping its version under an UPDATE publication" \sourceConfig -> do
        void $ prepareCatalog sourceConfig
        -- Restore the previous version-table shape and keep representative
        -- event data, so this exercises the upgrade path rather than bootstrap.
        runSQL sourceConfig do
          () <-
            PG.multiQE
              defaultTxErrorHandler
              [PG.sql|
                ALTER TABLE hdb_catalog.hdb_source_catalog_version REPLICA IDENTITY DEFAULT;
                UPDATE hdb_catalog.hdb_source_catalog_version SET version = '4';
                INSERT INTO hdb_catalog.event_log
                  (id, schema_name, table_name, trigger_name, payload)
                  VALUES ('retained-event', 'public', 'test', 'test', '{}'::jsonb);
                INSERT INTO hdb_catalog.event_invocation_logs (id, event_id, trigger_name)
                  VALUES ('retained-invocation', 'retained-event', 'test');
                INSERT INTO hdb_catalog.hdb_event_log_cleanups (id, trigger_name, scheduled_at, status)
                  VALUES ('retained-cleanup', 'test', NOW(), 'scheduled');
              |]
          pure ()
        createPublication sourceConfig
        (recreate, migrationState) <- prepareCatalog sourceConfig
        recreate `shouldBe` RETRecreate
        case migrationState of
          Version.SCMSMigratedTo 4 5 -> pure ()
          _ -> expectationFailure "Expected source catalog migration from version 4 to version 5"
        assertVersionAndIdentity sourceConfig
        runSQL sourceConfig retainedData `shouldReturn` [("retained-event", "retained-invocation", "retained-cleanup")]
        assertNoMigration sourceConfig
  where
    makeSourceConfig = do
      connInfo <- mkConnInfoWithFinalizer pgConnInfo (pure ())
      pure $ PGSourceConfig pgExecCtx connInfo Nothing (pure ()) defaultPostgresExtensionsSchema mempty ConnTemplate_NotApplicable

runSQL :: PGSourceConfig -> PG.TxET QErr IO a -> IO a
runSQL sourceConfig query =
  runPgSourceWriteTx sourceConfig InternalRawQuery query
    >>= either (fail . T.unpack . showQErr) pure

prepareCatalog :: PGSourceConfig -> IO (RecreateEventTriggers, Version.SourceCatalogMigrationState)
prepareCatalog sourceConfig =
  runExceptT (Source.prepareCatalog sourceConfig)
    >>= either (fail . T.unpack . showQErr) pure

resetCatalog :: PGSourceConfig -> IO ()
resetCatalog sourceConfig = runSQL sourceConfig do
  () <-
    PG.multiQE
      defaultTxErrorHandler
      [PG.sql|
        DROP PUBLICATION IF EXISTS source_catalog_update_test;
        DROP SCHEMA IF EXISTS hdb_catalog CASCADE;
      |]
  pure ()

createPublication :: PGSourceConfig -> IO ()
createPublication sourceConfig =
  runSQL sourceConfig
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
        CREATE PUBLICATION source_catalog_update_test FOR ALL TABLES WITH (publish = 'update')
      |]
      ()
      False

assertVersionAndIdentity :: PGSourceConfig -> Expectation
assertVersionAndIdentity sourceConfig =
  runSQL
    sourceConfig
    ( PG.withQE
        defaultTxErrorHandler
        [PG.sql|
          SELECT version, relreplident::text
          FROM hdb_catalog.hdb_source_catalog_version
          CROSS JOIN pg_class
          WHERE pg_class.oid = 'hdb_catalog.hdb_source_catalog_version'::regclass
        |]
        ()
        False
    )
    `shouldReturn` ([("5", "f")] :: [(Text, Text)])

assertNoMigration :: PGSourceConfig -> Expectation
assertNoMigration sourceConfig = do
  (recreate, migrationState) <- prepareCatalog sourceConfig
  recreate `shouldBe` RETDoNothing
  case migrationState of
    Version.SCMSNothingToDo 5 -> pure ()
    _ -> expectationFailure "Expected an already-current source catalog"

retainedData :: PG.TxET QErr IO [(Text, Text, Text)]
retainedData =
  PG.withQE
    defaultTxErrorHandler
    [PG.sql|
      SELECT event_log.id, event_invocation_logs.id, hdb_event_log_cleanups.id
      FROM hdb_catalog.event_log
      JOIN hdb_catalog.event_invocation_logs ON event_invocation_logs.event_id = event_log.id
      CROSS JOIN hdb_catalog.hdb_event_log_cleanups
    |]
    ()
    False
