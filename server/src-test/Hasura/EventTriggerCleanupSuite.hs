{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.EventTriggerCleanupSuite (buildEventTriggerCleanupSuite) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.DDL.EventTrigger
import Hasura.Backends.Postgres.Instances.Transport ()
import Hasura.Base.Error (QErr, showQErr)
import Hasura.Eventing.Common (cleanupSchedulesToBeGenerated)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (defaultSource)
import Hasura.RQL.Types.EventTrigger
import Hasura.SQL.Backend
import Hasura.Server.Init (considerEnv, databaseUrlOption, runWithEnv, _envVar)
import System.Cron (everyMinute)
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import Test.Hspec
import Text.Shakespeare.Text qualified as ST

buildEventTriggerCleanupSuite :: IO Spec
buildEventTriggerCleanupSuite = do
  env <- getEnvironment

  pgUrlText :: Text <- flip onLeft (printErrExit . T.pack) $
    runWithEnv env $ do
      let envVar = _envVar databaseUrlOption
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $ "Expected: " <> envVar

  let pgConnInfo = Q.ConnInfo 1 $ Q.CDDatabaseURI $ txtToBs pgUrlText

  pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams print

  let pgContext = mkPGExecCtx Q.ReadCommitted pgPool
      dbSourceConfig = PGSourceConfig pgContext pgConnInfo Nothing (pure ()) defaultPostgresExtensionsSchema

  pure $ do
    describe "Event trigger log cleanup" $ eventTriggerLogCleanupSpec dbSourceConfig

eventTriggerLogCleanupSpec :: SourceConfig ('Postgres 'Vanilla) -> Spec
eventTriggerLogCleanupSpec sourceConfig = do
  let setupDDLTx = do
        -- create schema hdb_catalog
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
            CREATE SCHEMA hdb_catalog;
          |]
          ()
          False
        -- create event_log table
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
          CREATE TABLE hdb_catalog.event_log
          (
            id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
            trigger_name TEXT NOT NULL,
            delivered BOOLEAN NOT NULL DEFAULT FALSE,
            error BOOLEAN NOT NULL DEFAULT FALSE,
            created_at TIMESTAMP DEFAULT NOW(),
            locked TIMESTAMPTZ
          );
          |]
          ()
          False
        -- create event_invocation_logs table
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
          CREATE TABLE hdb_catalog.event_invocation_logs
          (
            id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
            trigger_name TEXT,
            event_id TEXT
          );
          |]
          ()
          False
        -- create event_log_cleanups table
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
          CREATE TABLE hdb_catalog.hdb_event_log_cleanups
            (
              id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
              trigger_name TEXT NOT NULL,
              scheduled_at TIMESTAMP NOT NULL,
              deleted_event_logs INTEGER,
              deleted_event_invocation_logs INTEGER,
              status TEXT NOT NULL,
              CHECK (status IN ('scheduled', 'paused', 'completed', 'dead')),

              UNIQUE (trigger_name, scheduled_at)
            );
          |]
          ()
          False
      setupValues =
        -- insert few event logs and corresponding invocation logs
        -- We are inserting 5 logs which are past the retention time and 4 which are not
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
          WITH addedEventLogs AS (
            INSERT INTO hdb_catalog.event_log (trigger_name, delivered, created_at)
            VALUES
            ($1, true, now() - INTERVAL '9 hours'),
            ($1, true, now() - INTERVAL '8 hours'),
            ($1, true, now() - INTERVAL '7 hours'),
            ($1, true, now() - INTERVAL '6 hours'),
            ($1, true, now() - INTERVAL '5 hours'),
            ($1, true, now() - INTERVAL '3 hours'),
            ($1, true, now() - INTERVAL '2 hours'),
            ($1, true, now() - INTERVAL '1 hours'),
            ($1, true, now())
            RETURNING id
          )
          INSERT INTO hdb_catalog.event_invocation_logs (event_id)
          SELECT id as event_id FROM addedEventLogs;
          |]
          (Identity $ triggerNameToTxt triggerName)
          False

      teardownDDLTx =
        -- drop the schema
        Q.unitQE
          defaultTxErrorHandler
          [Q.sql|
              DROP SCHEMA IF EXISTS hdb_catalog CASCADE;
         |]
          ()
          False
  let setup = do
        -- drop everything that might affect the test
        runPgSourceWriteTx sourceConfig teardownDDLTx >>= (`onLeft` (printErrExit . showQErr))
        -- add tables
        runPgSourceWriteTx sourceConfig setupDDLTx >>= (`onLeft` (printErrExit . showQErr))
        -- insert some values in event log tables
        runPgSourceWriteTx sourceConfig setupValues >>= (`onLeft` (printErrExit . showQErr))

      teardown =
        runPgSourceWriteTx sourceConfig teardownDDLTx >>= (`onLeft` (printErrExit . showQErr))

      runSQLQuery :: Q.TxET QErr IO a -> IO a
      runSQLQuery = runExceptQErr . liftEitherM . liftIO . runPgSourceWriteTx sourceConfig

  describe "testing generator thread core logic: add cleanup schedules" $ do
    it "adding cleanup schedule" $ do
      -- run the setup
      liftIO setup
      -- run the core generator logic
      liftIO $ runExceptQErr $ addCleanupSchedules sourceConfig [(triggerName, autoTriggerCleanupConfig)]
      -- check if the cleanups are scheduled
      runSQLQuery (getCleanupStatusCount triggerName "scheduled") `shouldReturn` cleanupSchedulesToBeGenerated
      -- finally teardown
      liftIO teardown

  describe "testing consumer thread core logic" $ do
    it "testing getCleanupEventsForDeletion" $ do
      -- run the setup
      liftIO setup
      -- add some cleanup schedules
      liftIO $ runExceptQErr $ addCleanupSchedules sourceConfig [(triggerName, autoTriggerCleanupConfig)]
      -- move 11 minutes into the future, this should do the following:
      -- 1. render 10 cleanup schedules as dead
      -- 2. 1 schedule as ready to be delivered
      -- 3. 39 schedules as future schedules
      runSQLQuery $ reduceScheduledAtBy triggerName 11
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- liftIO $ runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      -- this should return a list of length 1 as we have only one event trigger
      length cleanupEventsForDeletion `shouldBe` 1
      (_, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- count the number of dead schedules
      runSQLQuery (getCleanupStatusCount triggerName "dead") `shouldReturn` 10
      -- count the number of completed schedules
      runSQLQuery (getCleanupStatusCount triggerName "completed") `shouldReturn` 0
      -- finally teardown
      liftIO teardown

    it "testing pausing logic" $ do
      -- run the setup
      liftIO setup
      -- add some cleanup schedules
      liftIO $ runExceptQErr $ addCleanupSchedules sourceConfig [(triggerName, autoTriggerCleanupConfig)]
      -- move 1 minute into the future
      runSQLQuery $ reduceScheduledAtBy triggerName 1
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- liftIO $ runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      (cleanupID, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- update the status to paused
      liftIO $ runExceptQErr $ updateCleanupEventStatusToPaused sourceConfig cleanupID
      -- count paused schedules
      runSQLQuery (getCleanupStatusCount triggerName "paused") `shouldReturn` 1
      -- finally teardown
      liftIO teardown

    it "testing completion logic" $ do
      -- run the setup
      liftIO setup
      -- add some cleanup schedules
      liftIO $ runExceptQErr $ addCleanupSchedules sourceConfig [(triggerName, autoTriggerCleanupConfig)]
      -- move 1 minute into the future
      runSQLQuery $ reduceScheduledAtBy triggerName 1
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- liftIO $ runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      (cleanupID, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- update the status to completed
      liftIO $ runExceptQErr $ updateCleanupEventStatusToCompleted sourceConfig cleanupID (DeletedEventLogStats 0 0)
      -- count completed schedules
      runSQLQuery (getCleanupStatusCount triggerName "completed") `shouldReturn` 1
      -- finally teardown
      liftIO teardown

    it "testing cleanup logic" $ do
      -- run the setup
      liftIO setup
      -- we have 5 logs which are past the retention period
      -- try deleting 2 event logs and invocation logs
      liftIO (runExceptQErr $ deleteEventTriggerLogs sourceConfig (triggerLogCleanupConfig True))
        `shouldReturn` (DeletedEventLogStats 2 2)
      -- we have 3 logs which are past the retention period
      -- try deleting 2 event logs and no invocation logs
      liftIO (runExceptQErr $ deleteEventTriggerLogs sourceConfig (triggerLogCleanupConfig False))
        `shouldReturn` (DeletedEventLogStats 2 0)
      -- we have 1 event log which is past the retention period
      -- try deleting 2 event logs and invocation logs (should delete only 1)
      liftIO (runExceptQErr $ deleteEventTriggerLogs sourceConfig (triggerLogCleanupConfig True))
        `shouldReturn` (DeletedEventLogStats 1 1)
      -- finally teardown
      liftIO teardown

-- * Event trigger cleanup configuration

-- | A trigger name
triggerName :: TriggerName
triggerName = TriggerName . mkNonEmptyTextUnsafe $ "someTrigger"

-- | An auto cleanup config sample
autoTriggerCleanupConfig :: AutoTriggerLogCleanupConfig
autoTriggerCleanupConfig =
  AutoTriggerLogCleanupConfig
    { _atlccBatchSize = 2,
      _atlccRetentionPeriod = 4,
      _atlccQueryTimeout = 60,
      _atlccCleanInvocationLogs = True,
      _atlccPaused = ETCSUnpaused,
      _atlccSchedule = everyMinute
    }

-- | A cleanup config
triggerLogCleanupConfig :: Bool -> TriggerLogCleanupConfig
triggerLogCleanupConfig shouldDelInv =
  TriggerLogCleanupConfig
    { tlccBatchSize = 2,
      tlccRetentionPeriod = 4,
      tlccQueryTimeout = 60,
      tlccCleanInvocationLogs = shouldDelInv,
      tlccEventTriggerName = triggerName,
      tlccSourceName = defaultSource
    }

-- * Utils

-- | Stringifies QErrs and throws them.
runExceptQErr :: MonadFail m => ExceptT QErr m a -> m a
runExceptQErr ex = runExceptT ex >>= (`onLeft` (fail . T.unpack . showQErr))

-- | Print QErr
printErrExit :: Text -> IO a
printErrExit = (*> exitFailure) . T.putStrLn

-- | Returns a count of cleanup schedules based on status
getCleanupStatusCount :: TriggerName -> Text -> Q.TxE QErr Int
getCleanupStatusCount triggername status =
  runIdentity . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
        SELECT count(*) FROM hdb_catalog.hdb_event_log_cleanups
        WHERE trigger_name = $1 AND status = $2;
      |]
      (triggerNameToTxt triggername, status)
      True

-- | Decreases some minutes from the cleanup schedules
reduceScheduledAtBy :: TriggerName -> Int -> Q.TxE QErr ()
reduceScheduledAtBy triggername interval =
  Q.unitQE
    defaultTxErrorHandler
    ( Q.fromText
        [ST.st|
        UPDATE  hdb_catalog.hdb_event_log_cleanups
        SET scheduled_at=(scheduled_at - INTERVAL '#{interval} minutes')
        WHERE trigger_name = '#{triggName}';
      |]
    )
    ()
    False
  where
    triggName = triggerNameToTxt triggername
