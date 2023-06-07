module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
    setupGlobalConfig,
    setupLogger,
    setupTestingMode,
  )
where

--------------------------------------------------------------------------------

import Data.Char qualified as Char
import Data.IORef
import Data.List qualified as List
import Database.PostgreSQL.Simple.Options qualified as Options
import Harness.Exceptions
import Harness.GraphqlEngine (startServerThread)
import Harness.Logging
import Harness.Services.Composed (mkTestServicesConfig, teardownServices)
import Harness.Test.BackendType (BackendType (..))
import Harness.TestEnvironment (GlobalFlags (..), GlobalTestEnvironment (..), Protocol (..), TestingMode (..), defaultGlobalFlags, stopServer)
import Hasura.Prelude
import System.Directory
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger qualified as FL
import Test.Hspec (Spec, SpecWith, aroundAllWith, runIO)
import Test.Hspec.Core.Spec (Item (..), filterForestWithLabels, mapSpecForest, modifyConfig)

--------------------------------------------------------------------------------

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
  servicesConfig <- mkTestServicesConfig logger
  globalFlags <- do
    gfTraceCommands <- maybe False read <$> lookupEnv "TRACE_COMMANDS"
    pure defaultGlobalFlags {gfTraceCommands}
  pure GlobalTestEnvironment {requestProtocol = HTTP, ..}

-- | tear down the shared server and services.
teardownTestEnvironment :: GlobalTestEnvironment -> IO ()
teardownTestEnvironment (GlobalTestEnvironment {server, servicesConfig}) = do
  stopServer server
  teardownServices servicesConfig

-- | allow setting log output type
setupLogger :: IO (Logger, IO ())
setupLogger = do
  env <- getEnvironment
  logType <- fromMaybe
    (pure $ FL.LogFileNoRotate "tests-hspec.log" 1024)
    do
      str <- lookup "HASURA_TEST_LOGTYPE" env
      case Char.toUpper <$> str of
        "STDOUT" -> Just $ pure $ FL.LogStdout 64
        "STDERR" -> Just $ pure $ FL.LogStderr 64
        "FILE" -> Just $ pure $ FL.LogFileNoRotate "tests-hspec.log" 1024
        _ | Just logfile <- "FILE=" `List.stripPrefix` str -> Just $ do
          let dir = takeDirectory logfile
          dirExists <- doesPathExist dir
          fileExists <- doesFileExist logfile
          unless
            (dirExists || fileExists)
            (error $ "(HASURA_TEST_LOGTYPE) Directory " ++ dir ++ " does not exist!")
          pure $ FL.LogFileNoRotate logfile 1024
        _ -> Nothing
  (logger', cleanupLogger) <- FL.newFastLogger logType
  return (flLogger logger', cleanupLogger)

setupTestingMode :: IO TestingMode
setupTestingMode = do
  environment <- getEnvironment
  lookupTestingMode environment `onLeft` error

hook :: (HasCallStack) => SpecWith GlobalTestEnvironment -> Spec
hook specs = do
  (testingMode, (logger, _cleanupLogger)) <-
    runIO
      $ readIORef globalConfigRef
      `onNothingM` do
        testingMode <- setupTestingMode
        (logger, cleanupLogger) <- setupLogger
        setupGlobalConfig testingMode (logger, cleanupLogger)
        pure (testingMode, (logger, cleanupLogger))

  modifyConfig (addLoggingFormatter logger)

  let shouldRunTest :: [String] -> Item x -> Bool
      shouldRunTest labels _ = case testingMode of
        TestEverything -> True
        TestBackend (DataConnector _) ->
          any (List.isInfixOf "DataConnector") labels
        TestBackend backendType -> any (List.isInfixOf (show backendType)) labels
        TestNoBackends -> True -- this is for catching "everything else"
        TestNewPostgresVariant {} -> "Postgres" `elem` labels

  aroundAllWith (const . bracket (setupTestEnvironment testingMode logger) teardownTestEnvironment)
    $ mapSpecForest (filterForestWithLabels shouldRunTest) (contextualizeLogger specs)

{-# NOINLINE globalConfigRef #-}
globalConfigRef :: IORef (Maybe (TestingMode, (Logger, IO ())))
globalConfigRef = unsafePerformIO $ newIORef Nothing

setupGlobalConfig :: TestingMode -> (Logger, IO ()) -> IO ()
setupGlobalConfig testingMode loggerCleanup =
  writeIORef globalConfigRef $ Just (testingMode, loggerCleanup)
