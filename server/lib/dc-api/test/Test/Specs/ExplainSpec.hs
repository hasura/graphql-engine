{-# LANGUAGE PatternSynonyms #-}

module Test.Specs.ExplainSpec (spec) where

import Control.Lens ((&), (?~))
import Hasura.Backends.DataConnector.API (Capabilities (..), ExplainResponse (..), QueryRequest (..), qFields, pattern TableQueryRequest)
import Test.AgentAPI (explain)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Sandwich (describe, shouldNotBe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

-- Note:
-- We currently simply check that for a basic query the explain plan is not empty.
-- There may be additional tests for explain-plans in HGE that we can leverage.
--
spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec TestData {..} _ = do
  describe "Explain API" do
    it "can generate an explain plan a query for a list of artists" $ do
      let query = artistsQueryRequest
      ExplainResponse {..} <- explain query
      _erQuery `shouldNotBe` ""
      _erLines `shouldNotBe` []
  where
    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdArtistsTableName mempty mempty mempty query Nothing
