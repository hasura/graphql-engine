{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Backend () where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J (Value)
import Hasura.Backends.DataWrapper.Adapter.Types qualified as Adapter
import Hasura.Backends.DataWrapper.Agent.Client qualified as Agent.Client
import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Backends.DataWrapper.IR.Expression (Expression, Operator)
import Hasura.Backends.DataWrapper.IR.Function qualified as Function (Name)
import Hasura.Backends.DataWrapper.IR.Name as Name (Name (unName))
import Hasura.Backends.DataWrapper.IR.OrderBy (OrderType)
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as Scalar (Type)
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as Scalar.Type (Type (..))
import Hasura.Backends.DataWrapper.IR.Scalar.Value qualified as Scalar (Value)
import Hasura.Backends.DataWrapper.IR.Table as Table (Name)
import Hasura.Base.Error (Code (ValidationFailed), QErr, throw400)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..), XDisable)
import Hasura.RQL.Types.Common as RQL (boolScalar, floatScalar, stringScalar)
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------

-- | An alias for '()' indicating that a particular associated type has not yet
-- been implemented for the 'DataWrapper' backend.
--
-- '()' is used (rather than a type with an empty data constructor) because it
-- comes with many of the instances that these associated types require.
--
-- This alias should /not/ be exported from this module, and it's only defined
-- for clarity.
type Unimplemented = ()

instance Backend 'DataWrapper where
  type SourceConfig 'DataWrapper = Adapter.SourceConfig
  type SourceConnConfiguration 'DataWrapper = Agent.Client.ConnSourceConfig

  type TableName 'DataWrapper = Table.Name
  type FunctionName 'DataWrapper = Function.Name
  type RawFunctionInfo 'DataWrapper = XDisable
  type FunctionArgType 'DataWrapper = XDisable
  type ConstraintName 'DataWrapper = Unimplemented
  type BasicOrderType 'DataWrapper = OrderType
  type NullsOrderType 'DataWrapper = Unimplemented
  type CountType 'DataWrapper = Unimplemented
  type Column 'DataWrapper = Column.Name
  type ScalarValue 'DataWrapper = Scalar.Value
  type ScalarType 'DataWrapper = Scalar.Type
  type SQLExpression 'DataWrapper = Expression
  type SQLOperator 'DataWrapper = Operator
  type BooleanOperators 'DataWrapper = Const XDisable
  type ExtraTableMetadata 'DataWrapper = Unimplemented

  type XComputedField 'DataWrapper = XDisable
  type XRelay 'DataWrapper = XDisable
  type XNodesAgg 'DataWrapper = XDisable
  type XNestedInserts 'DataWrapper = XDisable
  type XStreamingSubscription 'DataWrapper = XDisable

  functionArgScalarType :: FunctionArgType 'DataWrapper -> ScalarType 'DataWrapper
  functionArgScalarType = error "functionArgScalarType: not implemented for GraphQL Data Wrappers."

  isComparableType :: ScalarType 'DataWrapper -> Bool
  isComparableType = isNumType @'DataWrapper

  isNumType :: ScalarType 'DataWrapper -> Bool
  isNumType Scalar.Type.Number = True
  isNumType _ = False

  textToScalarValue :: Maybe Text -> ScalarValue 'DataWrapper
  textToScalarValue = error "textToScalarValue: not implemented for GraphQL Data Wrappers."

  parseScalarValue :: ScalarType 'DataWrapper -> J.Value -> Either QErr (ScalarValue 'DataWrapper)
  parseScalarValue = error "parseScalarValue: not implemented for GraphQL Data Wrappers."

  scalarValueToJSON :: ScalarValue 'DataWrapper -> J.Value
  scalarValueToJSON = error "scalarValueToJSON: not implemented for GraphQL Data Wrappers."

  functionToTable :: FunctionName 'DataWrapper -> TableName 'DataWrapper
  functionToTable = error "functionToTable: not implemented for GraphQL Data Wrappers."

  -- phil said this was cursed
  tableToFunction :: TableName 'DataWrapper -> FunctionName 'DataWrapper
  tableToFunction = coerce

  tableGraphQLName :: TableName 'DataWrapper -> Either QErr G.Name
  tableGraphQLName name =
    G.mkName (Name.unName name)
      `onNothing` throw400 ValidationFailed ("TableName " <> Name.unName name <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'DataWrapper -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: not implemented for GraphQL Data Wrappers."

  scalarTypeGraphQLName :: ScalarType 'DataWrapper -> Either QErr G.Name
  scalarTypeGraphQLName = \case
    Scalar.Type.String -> pure stringScalar
    Scalar.Type.Number -> pure floatScalar
    Scalar.Type.Bool -> pure boolScalar

  snakeCaseTableName :: TableName 'DataWrapper -> Text
  snakeCaseTableName = Name.unName
