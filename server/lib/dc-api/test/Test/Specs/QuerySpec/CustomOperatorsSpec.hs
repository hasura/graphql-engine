module Test.Specs.QuerySpec.CustomOperatorsSpec (spec) where

import Control.Lens ((&), (?~))
import Control.Monad (forM_)
import Control.Monad.List (guard)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API qualified as API
import Language.GraphQL.Draft.Syntax (Name (..))
import Test.AgentAPI (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Sandwich (describe, shouldBe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

toScalarType :: ColumnType -> Maybe ScalarType
toScalarType = \case
  ColumnTypeScalar scalarType -> Just scalarType
  _ -> Nothing

spec :: TestData -> ScalarTypesCapabilities -> AgentDatasetTestSpec
spec TestData {..} (ScalarTypesCapabilities scalarTypesCapabilities) = describe "Custom Operators in Queries" do
  describe "Top-level application of custom operators" do
    -- We run a list monad to identify test representatives,
    let items :: HashMap.HashMap (Name, ScalarType) (ColumnName, TableName, ColumnName, ScalarType)
        items =
          HashMap.fromList do
            API.TableInfo {_tiName, _tiColumns} <- _tdSchemaTables
            ColumnInfo {_ciName, _ciType} <- _tiColumns
            scalarType <- maybeToList $ toScalarType _ciType
            ScalarTypeCapabilities {_stcComparisonOperators} <- maybeToList $ HashMap.lookup scalarType scalarTypesCapabilities
            (operatorName, argType) <- HashMap.toList $ unComparisonOperators _stcComparisonOperators
            ColumnInfo {_ciName = anotherColumnName, _ciType = anotherColumnType} <- _tiColumns
            guard $ anotherColumnType == ColumnTypeScalar argType
            pure ((operatorName, scalarType), (_ciName, _tiName, anotherColumnName, argType))

    forM_ (HashMap.toList items) \((operatorName, columnType), (columnName, tableName, argColumnName, argType)) -> do
      -- Perform a select using the operator in a where clause
      let queryRequest =
            let fields = Data.mkFieldsMap [(unColumnName columnName, _tdColumnField tableName (unColumnName columnName))]
                query' = Data.emptyQuery & qFields ?~ fields
             in TableQueryRequest tableName mempty mempty mempty query' Nothing
          where' =
            ApplyBinaryComparisonOperator
              (CustomBinaryComparisonOperator (unName operatorName))
              (_tdCurrentComparisonColumn (unColumnName columnName) columnType)
              (AnotherColumnComparison $ ComparisonColumn CurrentTable (mkColumnSelector argColumnName) argType Nothing)
          query =
            queryRequest
              & qrQuery . qWhere ?~ where'
              & qrQuery . qLimit ?~ 1 -- No need to test actual results
      it (Text.unpack $ "ComparisonOperator " <> unName operatorName <> ": " <> getScalarType columnType <> " executes without an error") do
        result <- queryGuarded query
        -- Check that you get a success response
        Data.responseRows result `shouldBe` take 1 (Data.responseRows result)
