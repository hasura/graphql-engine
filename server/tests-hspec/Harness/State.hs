-- | State shared by tests. We intentionally use an abstract type to
-- wrap up the values we need for tests, with accessors. This way, the
-- tests are less liable to refactorings when we add or change the
-- state.
module Harness.State
  ( State (..),
    getServer,
    Server (..),
  )
where

import Control.Concurrent
import Data.Word
import Prelude (String)

-- | A state that's passed to all tests.
data State = State
  { server :: Server
  }

-- | Information about the graphql-engine server that we're working
-- with.
data Server = Server
  { -- | The port to connect on.
    port :: Word16,
    urlPrefix :: String,
    -- The full URI prefix e.g. http://localhost

    -- | The thread that the server is running on, so we can stop it later.
    threadId :: ThreadId
  }

getServer :: State -> Server
getServer (State {server}) = server
