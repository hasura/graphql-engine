-- | `SetupAction` used in `Fixture` for defining test setup and teardown
module Harness.Test.SetupAction
  ( SetupAction (..),
    clearMetadata,
    permitTeardownFail,
    noTeardown,
    setupPermissionsAction,
  )
where

import Control.Exception.Safe (catchAny)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (Permission, createPermissionMetadata, dropPermissionMetadata)
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

-- | a SetupAction with no teardown
noTeardown :: IO a -> SetupAction
noTeardown setup =
  SetupAction
    { setupAction = setup,
      teardownAction = \_ -> pure ()
    }

permitTeardownFail :: SetupAction -> SetupAction
permitTeardownFail SetupAction {teardownAction = ta, setupAction = sa} =
  SetupAction
    { setupAction = sa,
      teardownAction = (\a -> ta a `catchAny` \_ -> return ())
    }

-- | A setup/teardown action for permissions.
setupPermissionsAction :: [Permission] -> TestEnvironment -> SetupAction
setupPermissionsAction permissions testEnvironment =
  SetupAction
    { setupAction = for_ permissions \permission ->
        GraphqlEngine.postMetadata_ testEnvironment do
          createPermissionMetadata testEnvironment permission,
      teardownAction = const $ for_ (reverse permissions) \permission ->
        GraphqlEngine.postMetadata_ testEnvironment do
          dropPermissionMetadata testEnvironment permission
    }
