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
import Data.UUID (UUID)
import Data.Word
import Database.PostgreSQL.Simple.Options (Options)
import Harness.Logging.Messages
import Harness.Test.BackendType
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
    testingMode :: TestingMode
  }

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { -- | shared setup not related to a particular test
    globalEnvironment :: GlobalTestEnvironment,
    -- | connection details for the instance of HGE we're connecting to
    server :: Server,
    -- | a uuid generated for each test suite used to generate a unique
    -- `SchemaName`
    uniqueTestId :: UniqueTestId,
    -- | the main backend type of the test, if applicable (ie, where we are not
    -- testing `remote <-> remote` joins or someting similarly esoteric)
    backendTypeConfig :: Maybe BackendTypeConfig
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
