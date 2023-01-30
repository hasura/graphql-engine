module Test.Specs.ErrorSpec (spec) where

import Control.Lens ((&), (?~))
import Data.Aeson (Value (..))
import Hasura.Backends.DataConnector.API
import Test.AgentClient (queryExpectError)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Sandwich (describe, shouldBe)
import Test.TestHelpers (AgentTestSpec, it)

spec :: TestData -> SourceName -> Config -> a -> AgentTestSpec
spec TestData {..} sourceName config _capabilities = describe "Error Protocol" do
  it "returns a structured error when sending an invalid query" do
    receivedArtistsError <- queryExpectError sourceName config brokenQueryRequest
    _crType receivedArtistsError `shouldBe` UncaughtError
  where
    brokenQueryRequest :: QueryRequest
    brokenQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
          query =
            Data.emptyQuery
              & qFields ?~ fields
              & qWhere
                ?~ ApplyBinaryComparisonOperator
                  (CustomBinaryComparisonOperator "FOOBAR")
                  (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType)
                  (ScalarValue (Number 1) $ artistIdScalarType)
       in QueryRequest _tdArtistsTableName [] query

    artistIdScalarType = _tdFindColumnScalarType _tdArtistsTableName "ArtistId"
