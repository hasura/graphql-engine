module Test.ExplainSpec (spec) where

import Control.Lens ((&), (?~))
import Data.Aeson.KeyMap qualified as KeyMap
import Hasura.Backends.DataConnector.API (Capabilities (..), Config, ExplainResponse (..), QueryRequest (..), Routes (..), SourceName, qFields)
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it, shouldNotBe)
import Prelude

-- Note:
-- We currently simply check that for a basic query the explain plan is not empty.
-- There may be additional tests for explain-plans in HGE that we can leverage.
--
spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec api sourceName config _ = do
  describe "Explain API" do
    it "can generate an explain plan a query for a list of artists" $ do
      let query = artistsQueryRequest
      ExplainResponse {..} <- (api // _explain) sourceName config query
      _erQuery `shouldNotBe` ""
      _erLines `shouldNotBe` []

artistsQueryRequest :: QueryRequest
artistsQueryRequest =
  let fields = KeyMap.fromList [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.artistsTableName [] query
