module Test.Specs.QuerySpec (spec) where

import Data.Foldable (for_)
import Hasura.Backends.DataConnector.API
import Test.Data (TestData)
import Test.Sandwich (describe)
import Test.Specs.QuerySpec.AggregatesSpec qualified as AggregatesSpec
import Test.Specs.QuerySpec.BasicSpec qualified as BasicSpec
import Test.Specs.QuerySpec.CustomOperatorsSpec qualified as CustomOperatorsSpec
import Test.Specs.QuerySpec.FilteringSpec qualified as FilteringSpec
import Test.Specs.QuerySpec.ForeachSpec qualified as ForeachSpec
import Test.Specs.QuerySpec.OrderBySpec qualified as OrderBySpec
import Test.Specs.QuerySpec.RedactionSpec qualified as RedactionSpec
import Test.Specs.QuerySpec.RelationshipsSpec qualified as RelationshipsSpec
import Test.TestHelpers (AgentDatasetTestSpec)
import Prelude

spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec testData capabilities@Capabilities {..} = do
  describe "query API" do
    BasicSpec.spec testData
    CustomOperatorsSpec.spec testData _cScalarTypes
    FilteringSpec.spec testData _cComparisons
    OrderBySpec.spec testData capabilities
    for_ _cRelationships $ \_relationshipsCapabilities ->
      RelationshipsSpec.spec testData (_cComparisons >>= _ccSubqueryComparisonCapabilities)
    AggregatesSpec.spec testData _cRelationships
    for_ (_cQueries >>= _qcForeach) $ \_foreachCapabilities ->
      ForeachSpec.spec testData capabilities
    for_ (_cQueries >>= _qcRedaction) $ \_redactionCapabilities ->
      RedactionSpec.spec testData capabilities
