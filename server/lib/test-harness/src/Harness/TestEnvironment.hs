-- | TestEnvironment shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- testEnvironment.
module Harness.TestEnvironment
  ( TestEnvironment (..),
    GlobalTestEnvironment (..),
    GlobalFlags (..),
    Protocol (..),
    Server (..),
    TestingMode (..),
    TestingRole (..),
    UniqueTestId (..),
    debugger,
    getServer,
    getTestingMode,
    getBackendTypeConfig,
    focusFixtureLeft,
    focusFixtureRight,
    scalarTypeToText,
    graphQLTypeToText,
    serverUrl,
    stopServer,
    testLogTrace,
    testLogMessage,
    testLogShow,
    testLogHarness,
    getSchemaName,
    getSchemaNameInternal,
    defaultGlobalFlags,
    traceIf,
  )
where

import Control.Concurrent.Async qualified as Async
import Data.Aeson (ToJSON, Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Has
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Harness.Constants qualified as Constants
import Harness.GlobalTestEnvironment
import Harness.Http qualified as Http
import Harness.Logging.Messages
import Harness.Permissions.Types (Permission)
import Harness.Schema.Name
import Harness.Services.Composed qualified as Services
import Harness.Test.BackendType
import Harness.Test.CustomOptions qualified as Custom
import Harness.Test.FixtureName
import Harness.Test.ScalarType
import Harness.UniqueTestId
import Harness.Yaml
import Hasura.Prelude
import System.Process (readProcess)
import Text.Pretty.Simple

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { -- | shared setup not related to a particular test
    globalEnvironment :: GlobalTestEnvironment,
    -- | a uuid generated for each test suite used to generate a unique
    -- `SchemaName`
    uniqueTestId :: UniqueTestId,
    -- | the backend types of the tests
    fixtureName :: FixtureName,
    -- | The permissions we'd like to use for testing.
    permissions :: TestingRole,
    -- | Custom fixture-specific options.
    _options :: Custom.Options,
    -- Compatibility with the new, componentised fixtures:
    _postgraphqlInternal :: TestEnvironment -> Http.RequestHeaders -> Value -> IO Value,
    _postMetadataInternal :: TestEnvironment -> Int -> Http.RequestHeaders -> Value -> IO Value,
    _shouldReturnYamlFInternal :: TestEnvironment -> (Value -> IO Value) -> IO Value -> Value -> IO (),
    _getSchemaNameInternal :: TestEnvironment -> SchemaName
  }

scalarTypeToText :: TestEnvironment -> ScalarType -> Text
scalarTypeToText TestEnvironment {fixtureName} = case fixtureName of
  Backend BackendTypeConfig {backendScalarType} -> backendScalarType
  _ -> error "scalarTypeToText only currently defined for the `Backend` `FixtureName`"

graphQLTypeToText :: TestEnvironment -> ScalarType -> Text
graphQLTypeToText TestEnvironment {fixtureName} = case fixtureName of
  Backend BackendTypeConfig {backendGraphQLType} -> backendGraphQLType
  _ -> error "graphQLTypeToText only currently defined for the `Backend` `FixtureName`"

-- | The role we're going to use for testing. Either we're an admin, in which
-- case all permissions are implied, /or/ we're a regular user, in which case
-- the given permissions will be applied.
data TestingRole = Admin | NonAdmin [Permission]

instance Has Logger TestEnvironment where
  getter = logger . globalEnvironment
  modifier f x =
    x
      { globalEnvironment =
          (globalEnvironment x)
            { logger =
                f
                  ( logger $ globalEnvironment x
                  )
            }
      }

instance Has GlobalTestEnvironment TestEnvironment where
  getter = globalEnvironment
  modifier f x = x {globalEnvironment = f (globalEnvironment x)}

instance Has Services.TestServicesConfig TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f x = modifier (modifier @_ @GlobalTestEnvironment f) x

instance Has Services.HgeBinPath TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Has Services.PostgresServerUrl TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Has Services.PassthroughEnvVars TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Has Services.HgeServerInstance TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Show TestEnvironment where
  show TestEnvironment {globalEnvironment} =
    "<TestEnvironment: " ++ urlPrefix (server globalEnvironment) ++ ":" ++ show (port (server globalEnvironment)) ++ " >"

debugger :: TestEnvironment -> IO ()
debugger TestEnvironment {globalEnvironment} = do
  putStrLn "Test run paused. See the state of the world here:"
  print globalEnvironment

  _ <- readProcess "open" [serverUrl (server globalEnvironment)] ""
  putStrLn "Press enter to continue testing..."

  void getLine

-- | the `BackendTypeConfig` is used to decide which schema name to use
-- and for data connector capabilities
-- this will fail when used with 'Combine' - we should use `focusFixtureLeft`
-- and `focusFixtureRight` to solve this
getBackendTypeConfig :: TestEnvironment -> Maybe BackendTypeConfig
getBackendTypeConfig testEnvironment = case fixtureName testEnvironment of
  Backend db -> Just db
  _ -> Nothing

-- | in remote schema tests, we have two fixtures, but only want to talk about
-- one at a time
focusFixtureLeft :: TestEnvironment -> TestEnvironment
focusFixtureLeft testEnv =
  testEnv
    { fixtureName = case fixtureName testEnv of
        Combine l _ -> l
        _ -> error "Could not focus on left-hand FixtureName"
    }

-- | in remote schema tests, we have two fixtures, but only want to talk about
-- one at a time
focusFixtureRight :: TestEnvironment -> TestEnvironment
focusFixtureRight testEnv =
  testEnv
    { fixtureName = case fixtureName testEnv of
        Combine _ r -> r
        _ -> error "Could not focus on right-hand FixtureName"
    }

-- | Retrieve the 'Server' associated with some 'TestEnvironment'.
getServer :: TestEnvironment -> Server
getServer TestEnvironment {globalEnvironment} = server globalEnvironment

-- | Retrieve the 'TestingMode' associated with some 'TestEnvironment'
getTestingMode :: TestEnvironment -> TestingMode
getTestingMode = testingMode . globalEnvironment

-- | Forcibly stop a given 'Server'.
stopServer :: Server -> IO ()
stopServer Server {thread} = Async.cancel thread

-- | Log an unstructured trace string. Should only be used directly in specs,
-- not in the Harness modules.
{-# ANN testLogTrace ("HLINT: ignore" :: String) #-}
testLogTrace :: (TraceString a) => TestEnvironment -> a -> IO ()
testLogTrace testEnv =
  testLogMessage testEnv . logTrace

-- | Log a Show-able value trace string in tests. Should only be used directly
-- in specs, not in the Harness modules.
testLogShow :: (Show a) => TestEnvironment -> a -> IO ()
testLogShow testEnv =
  testLogTrace testEnv . pShowNoColor

-- | log a trace message happening in the Harness modules. Should only be used
-- in the Harness modules, not in Specs.
--
-- This should ideally be replaced with more specific logging functions.
testLogHarness :: (TraceString a) => TestEnvironment -> a -> IO ()
testLogHarness testEnv = testLogMessage testEnv . logHarness

-- Compatibility with the new, componentised fixtures:

-- | This enables late binding of 'postGraphql' on the test environment.
-- This makes 'TestEnvironment'-based specs more readily compatible with componontised fixtures.
--
-- This instance is somewhat subtle, in that the 'PostGraphql' function we return
-- has to constructed using the *current* test environment. Otherwise we'll break
-- the late binding and 'postGraphqlInternal' won't pick up updates to
-- permissions or protocols.
instance Has Services.PostGraphql TestEnvironment where
  getter testEnv = Services.PostGraphql $ _postgraphqlInternal testEnv testEnv
  modifier f testEnv =
    testEnv
      { _postgraphqlInternal =
          const (Services.getPostGraphql $ f (getter testEnv))
      }

-- | This enables late binding of 'postMetadata' on the test environment.
-- This makes 'TestEnvironment'-based specs more readily compatible with componontised fixtures.
--
-- This instance is somewhat subtle, in that the 'PostMetadata' function we return
-- has to constructed using the *current* test environment. Otherwise we'll break
-- the late binding and 'postMetadataInternal' won't pick up updates to
-- permissions or protocols.
instance Has Services.PostMetadata TestEnvironment where
  getter testEnv = Services.PostMetadata $ _postMetadataInternal testEnv testEnv
  modifier f testEnv =
    testEnv
      { _postMetadataInternal =
          const (Services.getPostMetadata $ f (getter testEnv))
      }

-- | This enables late binding of 'shouldReturnYaml' on the test environment.
-- This makes 'TestEnvironment'-based specs more readily compatible with componontised fixtures.
-- Same nuances hold for this as for 'PostGraphql'.
instance Has ShouldReturnYamlF TestEnvironment where
  getter testEnvironment = ShouldReturnYamlF (_shouldReturnYamlFInternal testEnvironment testEnvironment)
  modifier f testEnvironment = testEnvironment {_shouldReturnYamlFInternal = const (getShouldReturnYamlF $ f (getter testEnvironment))}

instance Has SchemaName TestEnvironment where
  getter testEnv = _getSchemaNameInternal testEnv testEnv
  modifier f testEnv = testEnv {_getSchemaNameInternal = const (f (getter testEnv))}

-- | Given a `TestEnvironment`, returns a `SchemaName` to use in the test, used
-- to separate out different test suites
--
-- This is used both in setup and teardown, and in individual tests
--
-- The `TestEnvironment` contains a `uniqueTestId` and `backendType`, from
-- which we decide what the `SchemaName` should be.
--
-- The backendType is only required so we make changes for BigQuery for now,
-- once we do this for all backends we'll just need the unique id.
--
-- For all other backends, we fall back to the Constants that were used before
getSchemaName :: TestEnvironment -> SchemaName
getSchemaName = getter

-- | exposed for use when creating a TestEnvironment
getSchemaNameInternal :: TestEnvironment -> SchemaName
getSchemaNameInternal testEnv = getSchemaNameByTestIdAndBackendType (fmap backendType $ getBackendTypeConfig testEnv) (uniqueTestId testEnv)
  where
    getSchemaNameByTestIdAndBackendType :: Maybe BackendType -> UniqueTestId -> SchemaName
    getSchemaNameByTestIdAndBackendType Nothing _ = SchemaName "hasura" -- the `Nothing` case is for tests with multiple schemas
    getSchemaNameByTestIdAndBackendType (Just BigQuery) uniqueTestId =
      SchemaName
        $ T.pack
        $ "hasura_test_"
        <> show uniqueTestId
    getSchemaNameByTestIdAndBackendType (Just Postgres) _ = SchemaName Constants.postgresDb
    getSchemaNameByTestIdAndBackendType (Just SQLServer) _ = SchemaName $ T.pack Constants.sqlserverDb
    getSchemaNameByTestIdAndBackendType (Just Citus) _ = SchemaName Constants.citusDb
    getSchemaNameByTestIdAndBackendType (Just Cockroach) _ = SchemaName Constants.cockroachDb
    getSchemaNameByTestIdAndBackendType (Just (DataConnector "sqlite")) _ = SchemaName "main"
    getSchemaNameByTestIdAndBackendType (Just (DataConnector _)) _ = SchemaName $ T.pack Constants.dataConnectorDb

-- | trace if flag is set in the test environment.
traceIf :: (ToJSON a) => (Show a) => TestEnvironment -> a -> a
traceIf testEnv value
  | gfTraceCommands (globalFlags (globalEnvironment testEnv)) =
      let pretty = TL.unpack (pShow (encodePretty value))
       in trace (show (uniqueTestId testEnv) <> ":\n" <> pretty) value
  | otherwise = value
