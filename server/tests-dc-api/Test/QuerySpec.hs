module Test.QuerySpec (spec) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Hasura.Backends.DataConnector.API (Capabilities (..), Config, Routes (..), SourceName)
import Servant.API (NamedRoutes)
import Servant.Client (Client)
import Test.Hspec
import Test.QuerySpec.AggregatesSpec qualified
import Test.QuerySpec.BasicSpec qualified
import Test.QuerySpec.RelationshipsSpec qualified
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec api sourceName config Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec api sourceName config
    when (isJust cRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec api sourceName config
    Test.QuerySpec.AggregatesSpec.spec api sourceName config
