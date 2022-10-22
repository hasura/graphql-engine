{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Types () where

import Data.Aeson
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection qualified as MSSQL
import Hasura.Backends.MSSQL.ToQuery ()
import Hasura.Backends.MSSQL.Types.Insert qualified as MSSQL (MSSQLExtraInsertData)
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Backends.MSSQL.Types.Update qualified as MSSQL (BackendUpdate)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'MSSQL where
  type SourceConfig 'MSSQL = MSSQL.MSSQLSourceConfig
  type SourceConnConfiguration 'MSSQL = MSSQL.MSSQLConnConfiguration
  type Identifier 'MSSQL = Text
  type TableName 'MSSQL = MSSQL.TableName
  type RawFunctionInfo 'MSSQL = Void
  type FunctionName 'MSSQL = MSSQL.FunctionName
  type FunctionArgType 'MSSQL = Void
  type ConstraintName 'MSSQL = Text
  type BasicOrderType 'MSSQL = MSSQL.Order
  type NullsOrderType 'MSSQL = MSSQL.NullsOrder
  type CountType 'MSSQL = MSSQL.Countable MSSQL.ColumnName
  type Column 'MSSQL = MSSQL.ColumnName
  type ScalarValue 'MSSQL = MSSQL.Value
  type ScalarType 'MSSQL = MSSQL.ScalarType
  type BooleanOperators 'MSSQL = MSSQL.BooleanOperators
  type SQLExpression 'MSSQL = MSSQL.Expression
  type SQLOperator 'MSSQL = MSSQL.Op

  type BackendUpdate 'MSSQL = MSSQL.BackendUpdate

  type ExtraTableMetadata 'MSSQL = [MSSQL.ColumnName] -- List of identity columns
  type ExtraInsertData 'MSSQL = MSSQL.MSSQLExtraInsertData

  type XComputedField 'MSSQL = XDisable
  type XRelay 'MSSQL = XDisable
  type XNodesAgg 'MSSQL = XEnable
  type XNestedInserts 'MSSQL = XDisable
  type XOnConflict 'MSSQL = XDisable

  functionArgScalarType :: FunctionArgType 'MSSQL -> ScalarType 'MSSQL
  functionArgScalarType = absurd

  isComparableType :: ScalarType 'MSSQL -> Bool
  isComparableType = MSSQL.isComparableType

  isNumType :: ScalarType 'MSSQL -> Bool
  isNumType = MSSQL.isNumType

  textToScalarValue :: Maybe Text -> ScalarValue 'MSSQL
  textToScalarValue = maybe ODBC.NullValue ODBC.TextValue

  parseScalarValue :: ScalarType 'MSSQL -> Value -> Either QErr (ScalarValue 'MSSQL)
  parseScalarValue = MSSQL.parseScalarValue

  -- TODO: Is this Postgres specific? Should it be removed from the class?
  scalarValueToJSON :: ScalarValue 'MSSQL -> Value
  scalarValueToJSON = error "Unexpected MSSQL error: calling scalarValueToJSON. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  functionToTable :: FunctionName 'MSSQL -> TableName 'MSSQL
  functionToTable = error "Unexpected MSSQL error: calling functionToTable. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  tableToFunction :: TableName 'MSSQL -> FunctionName 'MSSQL
  tableToFunction = MSSQL.tableName

  tableGraphQLName :: TableName 'MSSQL -> Either QErr G.Name
  tableGraphQLName = MSSQL.getGQLTableName

  functionGraphQLName :: FunctionName 'MSSQL -> Either QErr G.Name
  functionGraphQLName = error "Unexpected MSSQL error: calling functionGraphQLName. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  scalarTypeGraphQLName :: ScalarType 'MSSQL -> Either QErr G.Name
  scalarTypeGraphQLName = runExcept . MSSQL.mkMSSQLScalarTypeName

  snakeCaseTableName :: TableName 'MSSQL -> Text
  snakeCaseTableName = MSSQL.snakeCaseTableName
