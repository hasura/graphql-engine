module Test.Specs.ErrorSpec (spec) where

import Control.Lens ((&), (?~))
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
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField "ArtistId" _tdIntType), ("Name", _tdColumnField "Name" _tdStringType)]
          query =
            Data.emptyQuery
              & qFields ?~ fields
              & qWhere
                ?~ ApplyBinaryComparisonOperator
                  (CustomBinaryComparisonOperator "FOOBAR")
                  (ComparisonColumn CurrentTable (ColumnName "ArtistId") NumberTy)
                  (ScalarValue "1" StringTy)
       in QueryRequest _tdArtistsTableName [] query
