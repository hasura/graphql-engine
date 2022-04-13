module Test.QuerySpec (spec) where

import Control.Monad (when)
import Hasura.Backends.DataWrapper.API (Capabilities (..), Routes (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client)
import Test.Hspec
import Test.QuerySpec.BasicSpec qualified
import Test.QuerySpec.RelationshipsSpec qualified
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Capabilities -> Spec
spec api Capabilities {..} = do
  describe "query API" do
    Test.QuerySpec.BasicSpec.spec api
    when (dcRelationships) $
      Test.QuerySpec.RelationshipsSpec.spec api
