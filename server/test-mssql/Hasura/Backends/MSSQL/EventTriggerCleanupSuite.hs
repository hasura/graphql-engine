{-# LANGUAGE QuasiQuotes #-}

-- | MSSQL port of "Test.Hasura.EventTriggerCleanupSuite" (test-postgres). Exercises the
-- event-trigger log cleanup functions against a real MSSQL server, in particular
-- 'deleteEventTriggerLogsTx', whose trigger-name interpolation was previously
-- unescaped (see 'mssqlFmtLit').
module Hasura.Backends.MSSQL.EventTriggerCleanupSuite (spec) where

import Control.Exception.Base (bracket)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Database.MSSQL.Pool
import Database.MSSQL.Transaction qualified as MSTx
import Database.ODBC.SQLServer (rawUnescapedText, toSql)
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.DDL.EventTrigger
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Base.Error (QErr, showQErr)
import Hasura.Eventing.Common (cleanupSchedulesToBeGenerated)
import Hasura.Prelude
import Hasura.RQL.Types.Common (defaultSource)
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.ResizePool (ResizePoolStrategy (NeverResizePool))
import System.Cron (everyMinute)
import System.Exit (exitFailure)
import Test.Hspec
import Text.Shakespeare.Text qualified as ST

spec :: SpecWith ConnectionString
spec = describe "Event trigger log cleanup" eventTriggerLogCleanupSpec

eventTriggerLogCleanupSpec :: SpecWith ConnectionString
eventTriggerLogCleanupSpec = do
  describe "testing generator thread core logic: add cleanup schedules" $ do
    it "adding cleanup schedule" $ \connString -> withSourceConfig connString $ \sourceConfig -> do
      -- run the setup
      setupSchema sourceConfig
      -- run the core generator logic
      runExceptQErr $ addCleanupSchedules sourceConfig $ pure (triggerName, autoTriggerCleanupConfig)
      -- check if the cleanups are scheduled
      runSQLQuery sourceConfig (getCleanupStatusCount triggerName "scheduled") `shouldReturn` cleanupSchedulesToBeGenerated
      -- finally teardown
      teardownSchema sourceConfig

  describe "testing consumer thread core logic" $ do
    it "testing getCleanupEventsForDeletion" $ \connString -> withSourceConfig connString $ \sourceConfig -> do
      -- run the setup
      setupSchema sourceConfig
      -- add some cleanup schedules
      runExceptQErr $ addCleanupSchedules sourceConfig $ pure (triggerName, autoTriggerCleanupConfig)
      -- move 11 minutes into the future, this should do the following:
      -- 1. render 10 cleanup schedules as dead
      -- 2. 1 schedule as ready to be delivered
      -- 3. 39 schedules as future schedules
      runSQLQuery sourceConfig $ reduceScheduledAtBy triggerName 11
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      -- this should return a list of length 1 as we have only one event trigger
      length cleanupEventsForDeletion `shouldBe` 1
      (_, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- count the number of dead schedules
      runSQLQuery sourceConfig (getCleanupStatusCount triggerName "dead") `shouldReturn` 10
      -- count the number of completed schedules
      runSQLQuery sourceConfig (getCleanupStatusCount triggerName "completed") `shouldReturn` 0
      -- finally teardown
      teardownSchema sourceConfig

    it "testing pausing logic" $ \connString -> withSourceConfig connString $ \sourceConfig -> do
      -- run the setup
      setupSchema sourceConfig
      -- add some cleanup schedules
      runExceptQErr $ addCleanupSchedules sourceConfig $ pure (triggerName, autoTriggerCleanupConfig)
      -- move 1 minute into the future
      runSQLQuery sourceConfig $ reduceScheduledAtBy triggerName 1
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      (cleanupID, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- update the status to paused
      runExceptQErr $ updateCleanupEventStatusToPaused sourceConfig cleanupID
      -- count paused schedules
      runSQLQuery sourceConfig (getCleanupStatusCount triggerName "paused") `shouldReturn` 1
      -- finally teardown
      teardownSchema sourceConfig

    it "testing completion logic" $ \connString -> withSourceConfig connString $ \sourceConfig -> do
      -- run the setup
      setupSchema sourceConfig
      -- add some cleanup schedules
      runExceptQErr $ addCleanupSchedules sourceConfig $ pure (triggerName, autoTriggerCleanupConfig)
      -- move 1 minute into the future
      runSQLQuery sourceConfig $ reduceScheduledAtBy triggerName 1
      -- get cleanup actions to deliver
      cleanupEventsForDeletion <- runExceptQErr $ getCleanupEventsForDeletion sourceConfig
      (cleanupID, triggerNameLogToDelete) <-
        listToMaybe cleanupEventsForDeletion
          `onNothing` (error "expected a non empty list from getCleanupEventsForDeletion")
      triggerNameLogToDelete `shouldBe` triggerName
      -- update the status to completed
      runExceptQErr $ updateCleanupEventStatusToCompleted sourceConfig cleanupID (DeletedEventLogStats 0 0)
      -- count completed schedules
      runSQLQuery sourceConfig (getCleanupStatusCount triggerName "completed") `shouldReturn` 1
      -- finally teardown
      teardownSchema sourceConfig

    it "testing cleanup logic" $ \connString -> withSourceConfig connString $ \sourceConfig -> do
      -- run the setup
      setupSchema sourceConfig
      -- we have 5 logs which are past the retention period
      (runExceptQErr $ deleteEventTriggerLogs sourceConfig (triggerLogCleanupConfig True) (pure Nothing))
        `shouldReturn` (DeletedEventLogStats 5 5)
      -- we have 0 logs which are past the retention period now
      (runExceptQErr $ deleteEventTriggerLogs sourceConfig (triggerLogCleanupConfig False) (pure Nothing))
        `shouldReturn` (DeletedEventLogStats 0 0)
      -- finally teardown
      teardownSchema sourceConfig

-- * Connection setup

-- | Create a single-connection pool and an 'MSSQLSourceConfig' for the duration of the
-- given action, draining the pool afterwards.
withSourceConfig :: ConnectionString -> (MSSQLSourceConfig -> IO a) -> IO a
withSourceConfig connString action =
  bracket
    ( initMSSQLPool connString
        $ ConnectionOptionsPool
        $ PoolOptions
          { poConnections = 1,
            poStripes = 1,
            poIdleTime = 5
          }
    )
    drainMSSQLPool
    ( \pool ->
        action
          $ MSSQLSourceConfig connString (mkMSSQLExecCtx MSTx.ReadCommitted pool NeverResizePool) 0
    )

-- * Schema setup and teardown

-- | Minimal versions of the @hdb_catalog@ tables the cleanup functions operate on. Mirrors
-- the shapes used in @src-rsr/mssql/init_mssql_source.sql@.
setupDDLTx :: MSTx.TxE QErr ()
setupDDLTx = do
  MSTx.unitQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| CREATE SCHEMA hdb_catalog; |]
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      CREATE TABLE hdb_catalog.event_log
      (
        id UNIQUEIDENTIFIER NOT NULL PRIMARY KEY,
        trigger_name NVARCHAR(850) NOT NULL,
        delivered BIT NOT NULL DEFAULT 0,
        error BIT NOT NULL DEFAULT 0,
        created_at DATETIMEOFFSET(7) NOT NULL DEFAULT SYSDATETIMEOFFSET() AT TIME ZONE 'UTC',
        locked DATETIMEOFFSET(7)
      );
    |]
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      CREATE TABLE hdb_catalog.event_invocation_logs
      (
        id UNIQUEIDENTIFIER NOT NULL DEFAULT NEWID() PRIMARY KEY,
        trigger_name NVARCHAR(MAX),
        event_id UNIQUEIDENTIFIER
      );
    |]
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      CREATE TABLE hdb_catalog.hdb_event_log_cleanups
      (
        id UNIQUEIDENTIFIER NOT NULL DEFAULT NEWID() PRIMARY KEY,
        trigger_name NVARCHAR(900) NOT NULL,
        scheduled_at DATETIMEOFFSET(7) NOT NULL,
        deleted_event_logs INTEGER,
        deleted_event_invocation_logs INTEGER,
        status NVARCHAR(MAX) NOT NULL,
        CHECK (status IN ('scheduled', 'paused', 'completed', 'dead')),
        UNIQUE (trigger_name, scheduled_at)
      );
    |]

-- | Insert 9 event logs (with corresponding invocation logs) for 'triggerName': 5 which
-- are past the 4-hour retention window used by 'autoTriggerCleanupConfig' /
-- 'triggerLogCleanupConfig', and 4 which are not.
setupValuesTx :: MSTx.TxE QErr ()
setupValuesTx = do
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      INSERT INTO hdb_catalog.event_log (id, trigger_name, delivered, created_at)
      VALUES
        ('11111111-1111-1111-1111-111111111111', $triggerNameTxt, 1, DATEADD(HOUR, -9, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('22222222-2222-2222-2222-222222222222', $triggerNameTxt, 1, DATEADD(HOUR, -8, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('33333333-3333-3333-3333-333333333333', $triggerNameTxt, 1, DATEADD(HOUR, -7, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('44444444-4444-4444-4444-444444444444', $triggerNameTxt, 1, DATEADD(HOUR, -6, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('55555555-5555-5555-5555-555555555555', $triggerNameTxt, 1, DATEADD(HOUR, -5, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('66666666-6666-6666-6666-666666666666', $triggerNameTxt, 1, DATEADD(HOUR, -3, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('77777777-7777-7777-7777-777777777777', $triggerNameTxt, 1, DATEADD(HOUR, -2, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('88888888-8888-8888-8888-888888888888', $triggerNameTxt, 1, DATEADD(HOUR, -1, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC')),
        ('99999999-9999-9999-9999-999999999999', $triggerNameTxt, 1, SYSDATETIMEOFFSET() AT TIME ZONE 'UTC');
    |]
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      INSERT INTO hdb_catalog.event_invocation_logs (event_id)
      VALUES
        ('11111111-1111-1111-1111-111111111111'),
        ('22222222-2222-2222-2222-222222222222'),
        ('33333333-3333-3333-3333-333333333333'),
        ('44444444-4444-4444-4444-444444444444'),
        ('55555555-5555-5555-5555-555555555555'),
        ('66666666-6666-6666-6666-666666666666'),
        ('77777777-7777-7777-7777-777777777777'),
        ('88888888-8888-8888-8888-888888888888'),
        ('99999999-9999-9999-9999-999999999999');
    |]
  where
    triggerNameTxt = triggerNameToTxt triggerName

teardownDDLTx :: MSTx.TxE QErr ()
teardownDDLTx = do
  MSTx.unitQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| DROP TABLE IF EXISTS hdb_catalog.event_invocation_logs; |]
  MSTx.unitQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| DROP TABLE IF EXISTS hdb_catalog.event_log; |]
  MSTx.unitQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| DROP TABLE IF EXISTS hdb_catalog.hdb_event_log_cleanups; |]
  MSTx.unitQueryE HGE.defaultMSSQLTxErrorHandler [ODBC.sql| DROP SCHEMA IF EXISTS hdb_catalog; |]

setupSchema :: MSSQLSourceConfig -> IO ()
setupSchema sourceConfig = do
  -- drop everything that might affect the test, then recreate and populate
  runMSSQLSourceWriteTx sourceConfig teardownDDLTx >>= (`onLeft` (printErrExit . showQErr))
  runMSSQLSourceWriteTx sourceConfig setupDDLTx >>= (`onLeft` (printErrExit . showQErr))
  runMSSQLSourceWriteTx sourceConfig setupValuesTx >>= (`onLeft` (printErrExit . showQErr))

teardownSchema :: MSSQLSourceConfig -> IO ()
teardownSchema sourceConfig =
  runMSSQLSourceWriteTx sourceConfig teardownDDLTx >>= (`onLeft` (printErrExit . showQErr))

-- * Event trigger cleanup configuration

-- | A trigger name
triggerName :: TriggerName
triggerName = TriggerName . mkNonEmptyTextUnsafe $ "someTrigger"

-- | An auto cleanup config sample
autoTriggerCleanupConfig :: AutoTriggerLogCleanupConfig
autoTriggerCleanupConfig =
  AutoTriggerLogCleanupConfig
    { _atlccBatchSize = 2,
      _atlccClearOlderThan = 4,
      _atlccTimeout = 60,
      _atlccCleanInvocationLogs = True,
      _atlccPaused = ETCSUnpaused,
      _atlccSchedule = everyMinute
    }

-- | A cleanup config
triggerLogCleanupConfig :: Bool -> TriggerLogCleanupConfig
triggerLogCleanupConfig shouldDelInv =
  TriggerLogCleanupConfig
    { tlccBatchSize = 2,
      tlccClearOlderThan = 4,
      tlccTimeout = 60,
      tlccCleanInvocationLogs = shouldDelInv,
      tlccEventTriggerName = triggerName,
      tlccSourceName = defaultSource
    }

-- * Utils

-- | Stringifies QErrs and throws them.
runExceptQErr :: (MonadFail m) => ExceptT QErr m a -> m a
runExceptQErr ex = runExceptT ex >>= (`onLeft` (fail . T.unpack . showQErr))

-- | Print QErr
printErrExit :: Text -> IO a
printErrExit = (*> exitFailure) . T.putStrLn

runSQLQuery :: MSSQLSourceConfig -> MSTx.TxE QErr a -> IO a
runSQLQuery sourceConfig = runExceptQErr . liftEitherM . liftIO . runMSSQLSourceWriteTx sourceConfig

-- | Returns a count of cleanup schedules based on status
getCleanupStatusCount :: TriggerName -> Text -> MSTx.TxE QErr Int
getCleanupStatusCount triggername status =
  MSTx.singleRowQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      SELECT count(*) FROM hdb_catalog.hdb_event_log_cleanups
      WHERE trigger_name = $triggerNameTxt AND status = $status
    |]
  where
    triggerNameTxt = triggerNameToTxt triggername

-- | Decreases some minutes from the cleanup schedules
reduceScheduledAtBy :: TriggerName -> Int -> MSTx.TxE QErr ()
reduceScheduledAtBy triggername interval =
  MSTx.unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    ( rawUnescapedText
        [ST.st|
          UPDATE hdb_catalog.hdb_event_log_cleanups
          SET scheduled_at = DATEADD(MINUTE, -#{interval}, scheduled_at)
          WHERE trigger_name = #{triggNameLit};
        |]
    )
  where
    triggNameLit = mssqlFmtLit (triggerNameToTxt triggername)
