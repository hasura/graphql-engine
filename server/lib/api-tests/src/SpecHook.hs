module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
    setupGlobalConfig,
    setupLogType,
  )
where

import Data.Char qualified as Char
import Data.IORef
import Data.List qualified as List
import Data.Monoid (getLast)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.Options qualified as Options
import Harness.Constants qualified as Constants
import Harness.Exceptions (HasCallStack, bracket)
import Harness.GraphqlEngine (startServerThread)
import Harness.Logging
import Harness.Test.BackendType (BackendType (..))
import Harness.TestEnvironment (GlobalTestEnvironment (..), TestEnvironment (..), TestingMode (..), stopServer)
import Hasura.Prelude
import System.Environment (getEnvironment, lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger qualified as FL
import Test.Hspec (Spec, SpecWith, aroundAllWith, runIO)
import Test.Hspec.Core.Spec (Item (..), filterForestWithLabels, mapSpecForest, modifyConfig)

-- | Establish the mode in which we're running the tests. Currently, there are
-- four modes:
--
-- * @TestEverything@, which runs all the tests for all backends against the
--   credentials given in `Harness.Constants` from the `test-harness`.
--
-- * @TestBackend BackendType@, which only runs tests for the backend given
-- in @HASURA_TEST_BACKEND_TYPE@,
--
-- * @TestNoBackends@, for "everything else" that slips through the net
--
-- * @TestNewPostgresVariant@, which runs the Postgres tests against the
--   connection URI given in the @POSTGRES_VARIANT_URI@.
lookupTestingMode :: [(String, String)] -> Either String TestingMode
lookupTestingMode env =
  case lookup "POSTGRES_VARIANT_URI" env of
    Nothing ->
      case lookup "HASURA_TEST_BACKEND_TYPE" env of
        Nothing -> Right TestEverything
        Just backendType ->
          maybe (Left $ "Did not recognise backend type " <> backendType) Right (parseBackendType backendType)
    Just uri ->
      case Options.parseConnectionString uri of
        Left reason ->
          Left $ "Parsing variant URI failed: " ++ reason
        Right options ->
          Right $ TestNewPostgresVariant options

-- | which backend should we run tests for?
parseBackendType :: String -> Maybe TestingMode
parseBackendType backendType =
  case Char.toUpper <$> backendType of
    "POSTGRES" -> Just (TestBackend Postgres)
    "CITUS" -> Just (TestBackend Citus)
    "COCKROACH" -> Just (TestBackend Cockroach)
    "SQLSERVER" -> Just (TestBackend SQLServer)
    "BIGQUERY" -> Just (TestBackend BigQuery)
    "DATACONNECTOR" -> Just (TestBackend (DataConnector "all")) -- currently we ignore the exact DataConnector identifier and run them all
    "NONE" -> Just TestNoBackends
    _ -> Nothing

setupTestEnvironment :: TestingMode -> Logger -> IO GlobalTestEnvironment
setupTestEnvironment testingMode logger = do
  server <- startServerThread

  pure
    GlobalTestEnvironment
      { logger = logger,
        testingMode = testingMode,
        server = server
      }

-- | tear down the shared server
teardownTestEnvironment :: GlobalTestEnvironment -> IO ()
teardownTestEnvironment (GlobalTestEnvironment {server}) = stopServer server

-- | allow setting log output type
setupLogType :: IO FL.LogType
setupLogType = do
  env <- getEnvironment
  let defaultLogType = FL.LogFileNoRotate "tests-hspec.log" 1024
  pure case lookup "HASURA_TEST_LOGTYPE" env of
    Nothing -> defaultLogType
    Just str ->
      case Char.toUpper <$> str of
        "STDOUT" -> FL.LogStdout 64
        "STDERR" -> FL.LogStderr 64
        _ -> defaultLogType

hook :: HasCallStack => SpecWith GlobalTestEnvironment -> Spec
hook specs = do
  (testingMode, logType) <-
    runIO $
      readIORef globalConfigRef `onNothingM` do
        logType <- setupLogType
        environment <- getEnvironment
        testingMode <- lookupTestingMode environment `onLeft` error
        setupGlobalConfig testingMode logType
        pure (testingMode, logType)

  (logger', _cleanup) <- runIO $ FL.newFastLogger logType
  let logger = flLogger logger'

  modifyConfig (addLoggingFormatter logger)

  let shouldRunTest :: [String] -> Item x -> Bool
      shouldRunTest labels _ = case testingMode of
        TestEverything -> True
        TestBackend (DataConnector _) ->
          any (List.isInfixOf "DataConnector") labels
        TestBackend backendType -> show backendType `elem` labels
        TestNoBackends -> True -- this is for catching "everything else"
        TestNewPostgresVariant {} -> "Postgres" `elem` labels

  aroundAllWith (const . bracket (setupTestEnvironment testingMode logger) teardownTestEnvironment) $
    mapSpecForest (filterForestWithLabels shouldRunTest) (contextualizeLogger specs)

{-# NOINLINE globalConfigRef #-}
globalConfigRef :: IORef (Maybe (TestingMode, FL.LogType))
globalConfigRef = unsafePerformIO $ newIORef Nothing

setupGlobalConfig :: TestingMode -> FL.LogType -> IO ()
setupGlobalConfig testingMode logType =
  writeIORef globalConfigRef $ Just (testingMode, logType)
