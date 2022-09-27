module Test.QuerySpec (spec) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Hasura.Backends.DataConnector.API (Capabilities (..), ComparisonCapabilities (..), Config, Routes (..), SourceName)
import Servant.API (NamedRoutes)
import Servant.Client (Client)
import Test.Data (TestData)
import Test.Hspec
import Test.QuerySpec.AggregatesSpec qualified
import Test.QuerySpec.BasicSpec qualified
import Test.QuerySpec.FilteringSpec qualified
import Test.QuerySpec.OrderBySpec qualified
import Test.QuerySpec.RelationshipsSpec qualified
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec testData api sourceName config capabilities@Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec testData api sourceName config
    Test.QuerySpec.FilteringSpec.spec testData api sourceName config _cComparisons
    Test.QuerySpec.OrderBySpec.spec testData api sourceName config capabilities
    when (isJust _cRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec testData api sourceName config (_cComparisons >>= _ccSubqueryComparisonCapabilities)
    Test.QuerySpec.AggregatesSpec.spec testData api sourceName config _cRelationships
