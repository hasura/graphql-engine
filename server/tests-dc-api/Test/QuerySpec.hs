module Test.QuerySpec (spec) where

import Control.Monad (when)
import Hasura.Backends.DataConnector.API (Capabilities (..), Config, Routes (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client)
import Test.Hspec
import Test.QuerySpec.BasicSpec qualified
import Test.QuerySpec.RelationshipsSpec qualified
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Config -> Capabilities -> Spec
spec api config Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec api config
    when (dcRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec api config
