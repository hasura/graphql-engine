-- | State shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- state.
module Harness.State
  ( State (..),
    Server (..),
    getServer,
    serverUrl,
  )
where

import Control.Concurrent
import Data.Word
import Hasura.Prelude hiding (State)

-- | A state that's passed to all tests.
data State = State
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

-- | Retrieve the 'Server' associated with some 'State'.
getServer :: State -> Server
getServer State {server} = server

-- | Extracts the full URL prefix and port number from a given 'Server'.
--
-- @
--   > serverUrl (Server 8080 "http://localhost" someThreadId)
--   "http://localhost:8080"
-- @
serverUrl :: Server -> String
serverUrl Server {urlPrefix, port} = urlPrefix ++ ":" ++ show port
