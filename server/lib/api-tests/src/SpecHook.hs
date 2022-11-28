module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
  )
where

import Control.Exception.Safe (bracket)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Monoid (getLast)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.Options (Options (..), parseConnectionString)
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine (startServerThread)
import Harness.Logging
import Harness.Test.BackendType (BackendType (..))
import Harness.TestEnvironment (TestEnvironment (..), TestingMode (..), stopServer)
import Hasura.Prelude
import System.Environment (lookupEnv)
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
setupTestingMode :: IO TestingMode
setupTestingMode =
  lookupEnv "POSTGRES_VARIANT_URI" >>= \case
    Nothing ->
      lookupEnv "HASURA_TEST_BACKEND_TYPE" >>= \case
        Nothing -> pure TestEverything
        Just backendType ->
          onNothing (parseBackendType backendType) (error $ "Did not recognise backend type " <> backendType)
    Just uri ->
      case parseConnectionString uri of
        Left reason ->
          error $ "Parsing variant URI failed: " ++ reason
        Right options ->
          pure
            TestNewPostgresVariant
              { postgresSourceUser = fromMaybe Constants.postgresUser $ getLast (user options),
                postgresSourcePassword = fromMaybe Constants.postgresPassword $ getLast (password options),
                postgresSourceHost = fromMaybe Constants.postgresHost $ getLast (hostaddr options <> host options),
                postgresSourcePort = maybe Constants.defaultPostgresPort fromIntegral $ getLast (port options),
                postgresSourceInitialDatabase = fromMaybe Constants.postgresDb $ getLast (dbname options)
              }

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

setupTestEnvironment :: TestingMode -> Logger -> IO TestEnvironment
setupTestEnvironment testingMode logger = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  server <- startServerThread ((,) <$> murlPrefix <*> mport)
  uniqueTestId <- nextRandom
  pure
    TestEnvironment
      { server = server,
        uniqueTestId = uniqueTestId,
        backendType = Nothing,
        logger = logger,
        testingMode = testingMode
      }

teardownTestEnvironment :: TestEnvironment -> IO ()
teardownTestEnvironment TestEnvironment {..} = do
  stopServer server

-- | allow setting log output type
setupLogType :: IO (FL.LogType' FL.LogStr)
setupLogType =
  let defaultLogType = FL.LogFileNoRotate "tests-hspec.log" 1024
   in lookupEnv "HASURA_TEST_LOGTYPE" >>= \case
        Nothing -> pure defaultLogType
        Just str ->
          case Char.toUpper <$> str of
            "STDOUT" -> pure (FL.LogStdout 64)
            "STDERR" -> pure (FL.LogStderr 64)
            _ -> pure defaultLogType

hook :: SpecWith TestEnvironment -> Spec
hook specs = do
  logType <- runIO setupLogType
  (logger', _cleanup) <- runIO $ FL.newFastLogger logType
  let logger = flLogger logger'

  modifyConfig (addLoggingFormatter logger)

  testingMode <- runIO setupTestingMode

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
