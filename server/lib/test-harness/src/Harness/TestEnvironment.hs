-- | TestEnvironment shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- testEnvironment.
module Harness.TestEnvironment
  ( TestEnvironment (..),
    GlobalTestEnvironment (..),
    Server (..),
    TestingMode (..),
    UniqueTestId (..),
    getServer,
    getTestingMode,
    getBackendTypeConfig,
    focusFixtureLeft,
    focusFixtureRight,
    serverUrl,
    stopServer,
    testLogTrace,
    testLogMessage,
    testLogShow,
    testLogHarness,
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Data.Char qualified
import Data.Has
import Data.UUID (UUID)
import Data.Word
import Database.PostgreSQL.Simple.Options (Options)
import Harness.Logging.Messages
import Harness.Services.Composed qualified as Services
import Harness.Test.BackendType
import Harness.Test.FixtureName
import Hasura.Prelude
import Text.Pretty.Simple

newtype UniqueTestId = UniqueTestId {getUniqueTestId :: UUID}

-- | Sanitise UUID for use in BigQuery dataset name
-- must be alphanumeric (plus underscores)
instance Show UniqueTestId where
  show (UniqueTestId uuid) =
    fmap
      ( \a ->
          if Data.Char.isAlphaNum a
            then a
            else '_'
      )
      . show
      $ uuid

-- | static information across an entire test suite run
data GlobalTestEnvironment = GlobalTestEnvironment
  { -- | shared function to log information from tests
    logger :: Logger,
    -- | the mode in which we're running the tests. See 'TestingMode' for
    -- details'.
    testingMode :: TestingMode,
    -- | connection details for the instance of HGE we're connecting to
    server :: Server,
    servicesConfig :: Services.TestServicesConfig
  }

instance Has Logger GlobalTestEnvironment where
  getter = logger
  modifier f x = x {logger = f (logger x)}

instance Has GlobalTestEnvironment TestEnvironment where
  getter = globalEnvironment
  modifier f x = x {globalEnvironment = f (globalEnvironment x)}

instance Show GlobalTestEnvironment where
  show GlobalTestEnvironment {server} =
    "<GlobalTestEnvironment: " ++ urlPrefix server ++ ":" ++ show (port server) ++ " >"

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { -- | shared setup not related to a particular test
    globalEnvironment :: GlobalTestEnvironment,
    -- | a uuid generated for each test suite used to generate a unique
    -- `SchemaName`
    uniqueTestId :: UniqueTestId,
    -- | the backend types of the tests
    fixtureName :: FixtureName,
    -- | The role we attach to requests made within the tests. This allows us
    -- to test permissions.
    testingRole :: Maybe Text
  }

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

instance Has Services.TestServicesConfig GlobalTestEnvironment where
  getter = servicesConfig
  modifier f x = x {servicesConfig = f (servicesConfig x)}

instance Has Services.HgeBinPath GlobalTestEnvironment where
  getter = getter . getter @Services.TestServicesConfig
  modifier f = modifier (modifier @_ @Services.TestServicesConfig f)

instance Has Services.PostgresServerUrl GlobalTestEnvironment where
  getter = getter . getter @Services.TestServicesConfig
  modifier f = modifier (modifier @_ @Services.TestServicesConfig f)

instance Has Services.TestServicesConfig TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f x = modifier (modifier @_ @GlobalTestEnvironment f) x

instance Has Services.HgeBinPath TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Has Services.PostgresServerUrl TestEnvironment where
  getter = getter . getter @GlobalTestEnvironment
  modifier f = modifier (modifier @_ @GlobalTestEnvironment f)

instance Show TestEnvironment where
  show TestEnvironment {globalEnvironment} =
    "<TestEnvironment: " ++ urlPrefix (server globalEnvironment) ++ ":" ++ show (port (server globalEnvironment)) ++ " >"

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

-- | Credentials for our testing modes. See 'SpecHook.setupTestingMode' for the
-- practical consequences of this type.
data TestingMode
  = -- | run all tests, unfiltered
    TestEverything
  | -- | run only tests containing this BackendType (or a RemoteSchema, so
    -- those aren't missed)
    TestBackend BackendType
  | -- | run "all the other tests"
    TestNoBackends
  | -- | test a Postgres-compatible using a custom connection string
    TestNewPostgresVariant Options
  deriving (Eq, Ord, Show)

-- | Information about a server that we're working with.
data Server = Server
  { -- | The port to connect on.
    port :: Word16,
    -- | The full URI prefix e.g. http://localhost
    urlPrefix :: String,
    -- | The thread that the server is running on, so we can stop it later.
    thread :: Async ()
  }

-- | Retrieve the 'Server' associated with some 'TestEnvironment'.
getServer :: TestEnvironment -> Server
getServer TestEnvironment {globalEnvironment} = server globalEnvironment

-- | Extracts the full URL prefix and port number from a given 'Server'.
--
-- @
--   > serverUrl (Server 8080 "http://localhost" someThreadId)
--   "http://localhost:8080"
-- @
serverUrl :: Server -> String
serverUrl Server {urlPrefix, port} = urlPrefix ++ ":" ++ show port

-- | Retrieve the 'TestingMode' associated with some 'TestEnvironment'
getTestingMode :: TestEnvironment -> TestingMode
getTestingMode = testingMode . globalEnvironment

-- | Forcibly stop a given 'Server'.
stopServer :: Server -> IO ()
stopServer Server {thread} = Async.cancel thread

-- | Log a structured message in tests
testLogMessage :: LoggableMessage a => TestEnvironment -> a -> IO ()
testLogMessage = runLogger . logger . globalEnvironment

-- | Log an unstructured trace string. Should only be used directly in specs,
-- not in the Harness modules.
{-# ANN testLogTrace ("HLINT: ignore" :: String) #-}
testLogTrace :: TraceString a => TestEnvironment -> a -> IO ()
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
testLogHarness :: TraceString a => TestEnvironment -> a -> IO ()
testLogHarness testEnv = testLogMessage testEnv . logHarness
