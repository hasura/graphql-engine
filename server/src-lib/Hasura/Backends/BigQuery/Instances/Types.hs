{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Types where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax    as G

import           Data.Aeson

import qualified Hasura.Backends.BigQuery.Source  as BigQuery
import qualified Hasura.Backends.BigQuery.Types   as BigQuery

import           Hasura.Backends.BigQuery.ToQuery ()
import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend


instance Backend 'BigQuery where
  type SourceConfig            'BigQuery = BigQuery.BigQuerySourceConfig
  type SourceConnConfiguration 'BigQuery = BigQuery.BigQueryConnSourceConfig
  type Identifier              'BigQuery = Void
  type TableName               'BigQuery = BigQuery.TableName
  type FunctionName            'BigQuery = BigQuery.FunctionName
  type RawFunctionInfo         'BigQuery = Void
  type FunctionArgType         'BigQuery = Void
  type ConstraintName          'BigQuery = Void
  type BasicOrderType          'BigQuery = BigQuery.Order
  type NullsOrderType          'BigQuery = BigQuery.NullsOrder
  type CountType               'BigQuery = BigQuery.Countable BigQuery.ColumnName
  type Column                  'BigQuery = BigQuery.ColumnName
  type ScalarValue             'BigQuery = BigQuery.Value
  type ScalarType              'BigQuery = BigQuery.ScalarType
  type SQLExpression           'BigQuery = BigQuery.Expression
  type SQLOperator             'BigQuery = BigQuery.Op
  type BooleanOperators        'BigQuery = Const Void

  type XComputedField          'BigQuery = XDisable
  type XRelay                  'BigQuery = XDisable
  type XNodesAgg               'BigQuery = XEnable

  type ExtraTableMetadata      'BigQuery = ()

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
  snakeCaseTableName BigQuery.TableName { tableName, tableNameSchema } =
    tableNameSchema <> "_" <> tableName
