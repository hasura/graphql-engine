{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Types () where

import Data.Aeson qualified as J
import Data.Text.Casing qualified as C
import Database.MySQL.Base.Types qualified as MySQL
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.DDL.Headers ()
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'MySQL where
  type BackendConfig 'MySQL = ()
  type SourceConfig 'MySQL = MySQL.SourceConfig
  type SourceConnConfiguration 'MySQL = MySQL.ConnSourceConfig
  type TableName 'MySQL = MySQL.TableName
  type FunctionName 'MySQL = MySQL.FunctionName
  type RawFunctionInfo 'MySQL = Void -- MySQL.FunctionName
  type FunctionArgument 'MySQL = Void
  type ConstraintName 'MySQL = MySQL.ConstraintName
  type BasicOrderType 'MySQL = MySQL.Order
  type NullsOrderType 'MySQL = MySQL.NullsOrder
  type CountType 'MySQL = MySQL.Countable MySQL.Column
  type Column 'MySQL = MySQL.Column
  type ScalarValue 'MySQL = MySQL.ScalarValue
  type ScalarType 'MySQL = MySQL.ScalarType -- DB.Type
  type SQLExpression 'MySQL = MySQL.Expression
  type ScalarSelectionArguments 'MySQL = Void
  type BooleanOperators 'MySQL = Const Void
  type ComputedFieldDefinition 'MySQL = Void
  type FunctionArgumentExp 'MySQL = Const Void
  type ComputedFieldImplicitArguments 'MySQL = Void
  type ComputedFieldReturn 'MySQL = Void
  type XComputedField 'MySQL = Void
  type XRelay 'MySQL = Void
  type XNodesAgg 'MySQL = XEnable
  type ExtraTableMetadata 'MySQL = ()
  type XNestedInserts 'MySQL = XDisable
  type XStreamingSubscription 'MySQL = XDisable

  isComparableType :: ScalarType 'MySQL -> Bool
  isComparableType = isNumType @'MySQL -- TODO: For now we only allow comparisons for numeric types

  isNumType :: ScalarType 'MySQL -> Bool
  isNumType = \case
    MySQL.Decimal -> True
    MySQL.Tiny -> True
    MySQL.Short -> True
    MySQL.Long -> True
    MySQL.Float -> True
    MySQL.Double -> True
    MySQL.Null -> False
    MySQL.Timestamp -> False
    MySQL.LongLong -> True
    MySQL.Int24 -> True
    MySQL.Date -> False
    MySQL.Time -> False
    MySQL.DateTime -> False
    MySQL.Year -> False
    MySQL.NewDate -> False
    MySQL.VarChar -> False
    MySQL.Bit -> False
    MySQL.NewDecimal -> True
    MySQL.Enum -> False
    MySQL.Set -> False
    MySQL.TinyBlob -> False
    MySQL.MediumBlob -> False
    MySQL.LongBlob -> False
    MySQL.Blob -> False
    MySQL.VarString -> False
    MySQL.String -> False
    MySQL.Geometry -> False
    MySQL.Json -> False

  textToScalarValue :: Maybe Text -> ScalarValue 'MySQL
  textToScalarValue = error "textToScalarValue: MySQL backend does not support this operation yet."

  parseScalarValue :: ScalarType 'MySQL -> J.Value -> Either QErr (ScalarValue 'MySQL)
  parseScalarValue = error "parseScalarValue: MySQL backend does not support this operation yet."

  scalarValueToJSON :: ScalarValue 'MySQL -> J.Value
  scalarValueToJSON = error "scalarValueToJSON: MySQL backend does not support this operation yet."

  functionToTable :: FunctionName 'MySQL -> TableName 'MySQL
  functionToTable = error "functionToTable: MySQL backend does not support this operation yet."

  tableToFunction :: TableName 'MySQL -> FunctionName 'MySQL
  tableToFunction = MySQL.name

  tableGraphQLName :: TableName 'MySQL -> Either QErr G.Name
  tableGraphQLName MySQL.TableName {..} =
    let gName = maybe "" (<> "_") schema <> name
     in (G.mkName gName)
          `onNothing` throw400 ValidationFailed ("TableName " <> gName <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'MySQL -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: MySQL backend does not support this operation yet."

  scalarTypeGraphQLName :: ScalarType 'MySQL -> Either QErr G.Name
  scalarTypeGraphQLName = error "scalarTypeGraphQLName: MySQL backend does not support this operation yet."

  snakeCaseTableName :: TableName 'MySQL -> Text
  snakeCaseTableName MySQL.TableName {name, schema} =
    maybe "" (<> "_") schema <> name

  getTableIdentifier :: TableName 'MySQL -> Either QErr C.GQLNameIdentifier
  getTableIdentifier MySQL.TableName {..} = do
    let gName = maybe "" (<> "_") schema <> name
    gqlTableName <-
      (G.mkName gName)
        `onNothing` throw400 ValidationFailed ("TableName " <> gName <> " is not a valid GraphQL identifier")
    pure $ C.Identifier gqlTableName []

  namingConventionSupport :: SupportedNamingCase
  namingConventionSupport = OnlyHasuraCase

  computedFieldFunction :: ComputedFieldDefinition 'MySQL -> FunctionName 'MySQL
  computedFieldFunction = error "computedFieldFunction: MySQL backend does not support this operation yet"

  computedFieldReturnType :: ComputedFieldReturn 'MySQL -> ComputedFieldReturnType 'MySQL
  computedFieldReturnType = error "computedFieldReturnType: MySQL backend does not support this operation yet"

  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments 'MySQL -> [FunctionArgumentExp 'MySQL v]
  fromComputedFieldImplicitArguments = error "fromComputedFieldImplicitArguments: MySQL backend does not support this operation yet"
