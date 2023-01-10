-- | A test resource, providing functionality to a test via a fixture.
--
-- For example, this can be used to start a web server, and ensure it is shut
-- down cleanly when the test finishes.
--
-- For usage instructions, take a look at 'mkTestResource' and
-- 'AcquiredResource'.
module Harness.Test.TestResource
  ( Managed,
    mkTestResource,
    AcquiredResource (..),
  )
where

import Control.Monad.Managed (Managed, managed)
import Harness.Exceptions (bracket)
import Hasura.Prelude

-- | Constructs a test resource from an action setting up the resource.
--
-- Intended to be used with 'Fixture.mkLocalTestEnvironment' like this:
--
-- > mkLocalTestEnvironment = mkTestResource do
-- >   server <- startServerOn port
-- >   pure
-- >     AcquiredResource
-- >       { resourceValue = server,
-- >         waitForResource = healthCheck (urlOf server),
-- >         teardownResource = shutdown server
-- >       }
--
-- This returns a 'Managed a', which is monadic, and therefore can be composed
-- sequentially. This is especially useful when your setup step might fail. In
-- that case, consider composing two actions:
--
-- > mkLocalTestEnvironment = do
-- >   server <- mkTestResource do
-- >     server <- startServerOn port
-- >     pure
-- >       AcquiredResource
-- >         { resourceValue = server,
-- >           waitForResource = healthCheck (urlOf server),
-- >           teardownResource = shutdown server
-- >         }
-- >   client <- mkTestResource do
-- >     client <- connectTo port
-- >     pure
-- >       AcquiredResource
-- >         { resourceValue = client,
-- >           waitForResource = pure (),
-- >           teardownResource = shutdown client
-- >         }
mkTestResource :: IO (AcquiredResource a) -> Managed a
mkTestResource setupResource = managed $ \action ->
  bracket
    setupResource
    teardownResource
    \resource -> do
      waitForResource resource
      action $ resourceValue resource

-- | An 'AcquiredResource' represents a resource that takes up some space.
-- Typically, this is some kind of server, running on a port.
data AcquiredResource a = AcquiredResource
  { -- | The value of the acquired resource.
    resourceValue :: a,
    -- | Waits for the resource to be "healthy", and then returns.
    waitForResource :: IO (),
    -- | Shuts down the resource, freeing anything it's using.
    teardownResource :: IO ()
  }
