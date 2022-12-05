module Test.Specs.QuerySpec (spec) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Hasura.Backends.DataConnector.API (Capabilities (..), ComparisonCapabilities (..), Config, SourceName)
import Test.Data (TestData)
import Test.Sandwich (describe)
import Test.Specs.QuerySpec.AggregatesSpec qualified as Test.QuerySpec.AggregatesSpec
import Test.Specs.QuerySpec.BasicSpec qualified as Test.QuerySpec.BasicSpec
import Test.Specs.QuerySpec.FilteringSpec qualified as Test.QuerySpec.FilteringSpec
import Test.Specs.QuerySpec.OrderBySpec qualified as Test.QuerySpec.OrderBySpec
import Test.Specs.QuerySpec.RelationshipsSpec qualified as Test.QuerySpec.RelationshipsSpec
import Test.TestHelpers (AgentTestSpec)
import Prelude

spec :: TestData -> SourceName -> Config -> Capabilities -> AgentTestSpec
spec testData sourceName config capabilities@Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec testData sourceName config
    Test.QuerySpec.FilteringSpec.spec testData sourceName config _cComparisons
    Test.QuerySpec.OrderBySpec.spec testData sourceName config capabilities
    when (isJust _cRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec testData sourceName config (_cComparisons >>= _ccSubqueryComparisonCapabilities)
    Test.QuerySpec.AggregatesSpec.spec testData sourceName config _cRelationships
