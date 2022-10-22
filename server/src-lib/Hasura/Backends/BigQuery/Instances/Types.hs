{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Types () where

import Data.Aeson
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Hasura.Backends.BigQuery.Meta qualified as BigQuery
import Hasura.Backends.BigQuery.Source qualified as BigQuery
import Hasura.Backends.BigQuery.ToQuery ()
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'BigQuery where
  type BackendConfig 'BigQuery = ()
  type SourceConfig 'BigQuery = BigQuery.BigQuerySourceConfig
  type SourceConnConfiguration 'BigQuery = BigQuery.BigQueryConnSourceConfig
  type TableName 'BigQuery = BigQuery.TableName
  type FunctionName 'BigQuery = BigQuery.FunctionName
  type RawFunctionInfo 'BigQuery = BigQuery.RestRoutine
  type FunctionArgument 'BigQuery = BigQuery.FunctionArgument
  type ConstraintName 'BigQuery = Void
  type BasicOrderType 'BigQuery = BigQuery.Order
  type NullsOrderType 'BigQuery = BigQuery.NullsOrder
  type CountType 'BigQuery = BigQuery.Countable BigQuery.ColumnName
  type Column 'BigQuery = BigQuery.ColumnName
  type ScalarValue 'BigQuery = BigQuery.Value
  type ScalarType 'BigQuery = BigQuery.ScalarType
  type SQLExpression 'BigQuery = BigQuery.Expression
  type ScalarSelectionArguments 'BigQuery = Void
  type BooleanOperators 'BigQuery = BigQuery.BooleanOperators
  type ComputedFieldDefinition 'BigQuery = BigQuery.ComputedFieldDefinition
  type FunctionArgumentExp 'BigQuery = BigQuery.ArgumentExp
  type ComputedFieldImplicitArguments 'BigQuery = BigQuery.ComputedFieldImplicitArguments
  type ComputedFieldReturn 'BigQuery = BigQuery.ComputedFieldReturn

  type XStreamingSubscription 'BigQuery = XDisable
  type XComputedField 'BigQuery = XEnable
  type XRelay 'BigQuery = XDisable
  type XNodesAgg 'BigQuery = XEnable
  type XNestedInserts 'BigQuery = XDisable
  type XStreamingSubscription 'BigQuery = XDisable

  type ExtraTableMetadata 'BigQuery = ()

  isComparableType :: ScalarType 'BigQuery -> Bool
  isComparableType = BigQuery.isComparableType

  isNumType :: ScalarType 'BigQuery -> Bool
  isNumType = BigQuery.isNumType

  textToScalarValue :: Maybe Text -> ScalarValue 'BigQuery
  textToScalarValue = maybe BigQuery.NullValue BigQuery.StringValue

  parseScalarValue :: ScalarType 'BigQuery -> Value -> Either QErr (ScalarValue 'BigQuery)
  parseScalarValue = BigQuery.parseScalarValue

  scalarValueToJSON :: ScalarValue 'BigQuery -> Value
  scalarValueToJSON = error "scalarValueToJSON"

  functionToTable :: FunctionName 'BigQuery -> TableName 'BigQuery
  functionToTable = error "functionToTable"

  tableToFunction :: TableName 'BigQuery -> FunctionName 'BigQuery
  tableToFunction BigQuery.TableName {..} =
    BigQuery.FunctionName
      { functionName = tableName,
        functionNameSchema = Just tableNameSchema
      }

  tableGraphQLName :: TableName 'BigQuery -> Either QErr G.Name
  tableGraphQLName = BigQuery.getGQLTableName

  functionGraphQLName :: FunctionName 'BigQuery -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName"

  scalarTypeGraphQLName :: ScalarType 'BigQuery -> Either QErr G.Name
  scalarTypeGraphQLName = BigQuery.scalarTypeGraphQLName

  snakeCaseTableName :: TableName 'BigQuery -> Text
  snakeCaseTableName BigQuery.TableName {tableName, tableNameSchema} =
    tableNameSchema <> "_" <> tableName

  getTableIdentifier :: TableName 'BigQuery -> Either QErr GQLNameIdentifier
  getTableIdentifier tName = do
    gqlTableName <- BigQuery.getGQLTableName tName
    pure $ C.Identifier gqlTableName []

  namingConventionSupport :: SupportedNamingCase
  namingConventionSupport = OnlyHasuraCase

  computedFieldFunction :: ComputedFieldDefinition 'BigQuery -> FunctionName 'BigQuery
  computedFieldFunction = BigQuery._bqcfdFunction

  computedFieldReturnType :: ComputedFieldReturn 'BigQuery -> ComputedFieldReturnType 'BigQuery
  computedFieldReturnType = \case
    BigQuery.ReturnExistingTable tableName -> ReturnsTable tableName
    BigQuery.ReturnTableSchema _ -> ReturnsOthers

  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments 'BigQuery -> [FunctionArgumentExp 'BigQuery v]
  fromComputedFieldImplicitArguments _ _ =
    -- As of now, computed fields are not supported in boolean and order by expressions.
    -- We don't have to generate arguments expression from implicit arguments.
    []
