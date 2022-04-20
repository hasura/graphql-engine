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
  )
where

import Control.Concurrent (ThreadId, killThread)
import Data.Word
import Hasura.Prelude

-- | A testEnvironment that's passed to all tests.
data TestEnvironment = TestEnvironment
  { server :: Server
  }

-- | Information about a server that we're working with.
data Server = Server
  { -- | The port to connect on.
    port :: Word16,
    -- | The full URI prefix e.g. http://localhost
    urlPrefix :: String,
    -- | The thread that the server is running on, so we can stop it later.
    threadId :: ThreadId
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
stopServer Server {threadId} = killThread threadId
