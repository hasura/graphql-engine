-- | TestEnvironment shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- testEnvironment.
module Harness.TestEnvironment
  ( TestEnvironment (..),
    Server (..),
    TestingMode (..),
    getServer,
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
import Data.UUID (UUID)
import Data.Word
import Harness.Logging.Messages
import Harness.Test.BackendType
import Hasura.Prelude
import Text.Pretty.Simple

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { -- | connection details for the instance of HGE we're connecting to
    server :: Server,
    -- | shared function to log information from tests
    logger :: Logger,
    -- | a uuid generated for each test suite used to generate a unique
    -- `SchemaName`
    uniqueTestId :: UUID,
    -- | the main backend type of the test, if applicable (ie, where we are not
    -- testing `remote <-> remote` joins or someting similarly esoteric)
    backendType :: Maybe BackendType,
    -- | the mode in which we're running the tests. See 'TestingMode' for
    -- details'.
    testingMode :: TestingMode
  }

instance Show TestEnvironment where
  show TestEnvironment {server} = "<TestEnvironment: " ++ urlPrefix server ++ ":" ++ show (port server) ++ " >"

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
    TestNewPostgresVariant
      { postgresSourceUser :: String,
        postgresSourcePassword :: String,
        postgresSourceHost :: String,
        postgresSourcePort :: Word16,
        postgresSourceInitialDatabase :: String
      }
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
getServer TestEnvironment {server} = server

-- | Extracts the full URL prefix and port number from a given 'Server'.
--
-- @
--   > serverUrl (Server 8080 "http://localhost" someThreadId)
--   "http://localhost:8080"
-- @
serverUrl :: Server -> String
serverUrl Server {urlPrefix, port} = urlPrefix ++ ":" ++ show port

-- | Forcibly stop a given 'Server'.
stopServer :: Server -> IO ()
stopServer Server {thread} = Async.cancel thread

-- | Log a structured message in tests
testLogMessage :: LoggableMessage a => TestEnvironment -> a -> IO ()
testLogMessage testEnv = runLogger (logger testEnv)

-- | Log an unstructured trace string. Should only be used directly in specs,
-- not in the Harness modules.
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
