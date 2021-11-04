{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Types () where

import Data.Aeson
import Hasura.Backends.BigQuery.Source qualified as BigQuery
import Hasura.Backends.BigQuery.ToQuery ()
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'BigQuery where
  type SourceConfig 'BigQuery = BigQuery.BigQuerySourceConfig
  type SourceConnConfiguration 'BigQuery = BigQuery.BigQueryConnSourceConfig
  type Identifier 'BigQuery = Void
  type TableName 'BigQuery = BigQuery.TableName
  type FunctionName 'BigQuery = BigQuery.FunctionName
  type RawFunctionInfo 'BigQuery = Void
  type FunctionArgType 'BigQuery = Void
  type ConstraintName 'BigQuery = Void
  type BasicOrderType 'BigQuery = BigQuery.Order
  type NullsOrderType 'BigQuery = BigQuery.NullsOrder
  type CountType 'BigQuery = BigQuery.Countable BigQuery.ColumnName
  type Column 'BigQuery = BigQuery.ColumnName
  type ScalarValue 'BigQuery = BigQuery.Value
  type ScalarType 'BigQuery = BigQuery.ScalarType
  type SQLExpression 'BigQuery = BigQuery.Expression
  type SQLOperator 'BigQuery = BigQuery.Op
  type BooleanOperators 'BigQuery = Const Void

  type XComputedField 'BigQuery = XDisable
  type XRelay 'BigQuery = XDisable
  type XNodesAgg 'BigQuery = XEnable
  type XNestedInserts 'BigQuery = XDisable
  type XOnConflict 'BigQuery = XDisable

  type ExtraTableMetadata 'BigQuery = ()
  type ExtraInsertData 'BigQuery = ()

  functionArgScalarType :: FunctionArgType 'BigQuery -> ScalarType 'BigQuery
  functionArgScalarType = absurd

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
  tableToFunction = coerce . BigQuery.tableName

  tableGraphQLName :: TableName 'BigQuery -> Either QErr G.Name
  tableGraphQLName = BigQuery.getGQLTableName

  functionGraphQLName :: FunctionName 'BigQuery -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName"

  scalarTypeGraphQLName :: ScalarType 'BigQuery -> Either QErr G.Name
  scalarTypeGraphQLName = BigQuery.scalarTypeGraphQLName

  snakeCaseTableName :: TableName 'BigQuery -> Text
  snakeCaseTableName BigQuery.TableName {tableName, tableNameSchema} =
    tableNameSchema <> "_" <> tableName
