-- | TestEnvironment shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- testEnvironment.
module Harness.TestEnvironment
  ( TestEnvironment (..),
    Server (..),
    getServer,
    serverUrl,
    stopServer,
    testLog,
    testLogShow,
    testLogBytestring,
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Data.ByteString.Lazy qualified as LBS
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.UUID (UUID)
import Data.Word
import Harness.Test.BackendType
import Hasura.Prelude
import System.Log.FastLogger qualified as FL

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { -- | connection details for the instance of HGE we're connecting to
    server :: Server,
    -- | shared function to log information from tests
    logger :: FL.LogStr -> IO (),
    -- | action to clean up logger
    loggerCleanup :: IO (),
    -- | a uuid generated for each test suite used to generate a unique
    -- `SchemaName`
    uniqueTestId :: UUID,
    -- | the main backend type of the test, if applicable (ie, where we are not
    -- testing `remote <-> remote` joins or someting similarly esoteric)
    backendType :: Maybe BackendType
  }

instance Show TestEnvironment where
  show TestEnvironment {server} = "<TestEnvironment: " ++ urlPrefix server ++ ":" ++ show (port server) ++ " >"

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

-- | log a string out in tests
testLog :: TestEnvironment -> String -> IO ()
testLog testEnv =
  logger testEnv . fromString . (<>) "\n"

-- | log a Show-able value in tests
testLogShow :: (Show a) => TestEnvironment -> a -> IO ()
testLogShow testEnv =
  testLog testEnv . show

-- | log a UTF-8 Bytestring. Forgive me Padre for converting through String
testLogBytestring :: TestEnvironment -> LBS.ByteString -> IO ()
testLogBytestring testEnv =
  testLog testEnv . T.unpack . decodeUtf8 . LBS.toStrict
