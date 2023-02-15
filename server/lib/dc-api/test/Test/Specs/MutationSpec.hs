module Test.Specs.MutationSpec
  ( spec,
  )
where

import Data.Foldable (for_)
import Hasura.Backends.DataConnector.API
import Test.Data (TestData)
import Test.Sandwich (describe)
import Test.Specs.MutationSpec.InsertSpec qualified as InsertSpec
import Test.TestHelpers (AgentTestSpec)
import Prelude

spec :: TestData -> Capabilities -> AgentTestSpec
spec testData capabilities@Capabilities {..} = do
  describe "mutation API" do
    for_ (_cMutations >>= _mcInsertCapabilities) $ \_insertCapabilities ->
      InsertSpec.spec testData capabilities
