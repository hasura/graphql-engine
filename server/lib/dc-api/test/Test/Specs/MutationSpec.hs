module Test.Specs.MutationSpec
  ( spec,
  )
where

import Data.Foldable (for_)
import Hasura.Backends.DataConnector.API
import Test.Data (EdgeCasesTestData, TestData)
import Test.Sandwich (describe)
import Test.Specs.MutationSpec.DeleteSpec qualified as DeleteSpec
import Test.Specs.MutationSpec.InsertSpec qualified as InsertSpec
import Test.Specs.MutationSpec.UpdateSpec qualified as UpdateSpec
import Test.TestHelpers (AgentTestSpec)
import Prelude

spec :: TestData -> Maybe EdgeCasesTestData -> Capabilities -> AgentTestSpec
spec testData edgeCasesTestData capabilities@Capabilities {..} = do
  describe "mutation API" do
    for_ (_cMutations >>= _mcInsertCapabilities) $ \_insertCapabilities -> do
      InsertSpec.spec testData edgeCasesTestData capabilities
      UpdateSpec.spec testData edgeCasesTestData capabilities
      DeleteSpec.spec testData edgeCasesTestData capabilities
