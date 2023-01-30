module Test.Specs.ExplainSpec (spec) where

import Control.Lens ((&), (?~))
import Hasura.Backends.DataConnector.API (Capabilities (..), Config, ExplainResponse (..), QueryRequest (..), SourceName, qFields)
import Test.AgentClient (explain)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Sandwich (describe, shouldNotBe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

-- Note:
-- We currently simply check that for a basic query the explain plan is not empty.
-- There may be additional tests for explain-plans in HGE that we can leverage.
--
spec :: TestData -> SourceName -> Config -> Capabilities -> AgentTestSpec
spec TestData {..} sourceName config _ = do
  describe "Explain API" do
    it "can generate an explain plan a query for a list of artists" $ do
      let query = artistsQueryRequest
      ExplainResponse {..} <- explain sourceName config query
      _erQuery `shouldNotBe` ""
      _erLines `shouldNotBe` []
  where
    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [] query
