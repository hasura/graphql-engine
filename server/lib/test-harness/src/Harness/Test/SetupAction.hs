-- | `SetupAction` used in `Fixture` for defining test setup and teardown
module Harness.Test.SetupAction
  ( SetupAction (..),
    clearMetadata,
  )
where

import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Prelude

-- | a 'SetupAction' encodes how to setup and tear down a single piece of test
-- system state.
--
-- The value produced by a 'setupAction' is to be input into the corresponding
-- 'teardownAction', if the 'setupAction' completed without throwing an
-- exception.
data SetupAction = forall a.
  SetupAction
  { setupAction :: IO a,
    teardownAction :: Maybe a -> IO ()
  }

clearMetadata :: TestEnvironment -> SetupAction
clearMetadata testEnv =
  SetupAction
    { setupAction = GraphqlEngine.clearMetadata testEnv,
      teardownAction = \_ -> GraphqlEngine.clearMetadata testEnv
    }
