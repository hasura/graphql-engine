module Test.ErrorSpec (spec) where

import Control.Lens ((&), (?~))
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client)
import Test.Data (TestData (..), errorQuery)
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> a -> Spec
spec TestData {..} api sourceName config _capabilities = describe "Basic Queries" do
  describe "Error Protocol" do
    it "returns a structured error when sending an invalid query" do
      receivedArtistsError <- errorQuery api sourceName config brokenQueryRequest
      _crType receivedArtistsError `shouldBe` UncaughtError
  where
    brokenQueryRequest :: QueryRequest
    brokenQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField "ArtistId"), ("Name", _tdColumnField "Name")]
          query =
            Data.emptyQuery
              & qFields ?~ fields
              & qWhere
                ?~ ApplyBinaryComparisonOperator
                  (CustomBinaryComparisonOperator "FOOBAR")
                  (ComparisonColumn CurrentTable (ColumnName "ArtistId"))
                  (ScalarValue "1")
       in QueryRequest _tdArtistsTableName [] query
