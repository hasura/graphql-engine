module Test.Specs.QuerySpec (spec) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Hasura.Backends.DataConnector.API (Capabilities (..), ComparisonCapabilities (..))
import Test.Data (TestData)
import Test.Sandwich (describe)
import Test.Specs.QuerySpec.AggregatesSpec qualified as Test.QuerySpec.AggregatesSpec
import Test.Specs.QuerySpec.BasicSpec qualified as Test.QuerySpec.BasicSpec
import Test.Specs.QuerySpec.CustomOperatorsSpec qualified as Test.QuerySpec.CustomOperatorsSpec
import Test.Specs.QuerySpec.FilteringSpec qualified as Test.QuerySpec.FilteringSpec
import Test.Specs.QuerySpec.OrderBySpec qualified as Test.QuerySpec.OrderBySpec
import Test.Specs.QuerySpec.RelationshipsSpec qualified as Test.QuerySpec.RelationshipsSpec
import Test.TestHelpers (AgentDatasetTestSpec)
import Prelude

spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec testData capabilities@Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec testData
    Test.QuerySpec.CustomOperatorsSpec.spec testData _cScalarTypes
    Test.QuerySpec.FilteringSpec.spec testData _cComparisons
    Test.QuerySpec.OrderBySpec.spec testData capabilities
    when (isJust _cRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec testData (_cComparisons >>= _ccSubqueryComparisonCapabilities)
    Test.QuerySpec.AggregatesSpec.spec testData _cRelationships
