{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Backend () where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J (Value)
import Hasura.Backends.DataWrapper.Adapter.Types qualified as Adapter
import Hasura.Backends.DataWrapper.IR.Column qualified as IR.C
import Hasura.Backends.DataWrapper.IR.Expression qualified as IR.E
import Hasura.Backends.DataWrapper.IR.Function qualified as IR.F
import Hasura.Backends.DataWrapper.IR.Name qualified as IR.N
import Hasura.Backends.DataWrapper.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataWrapper.IR.Scalar.Value qualified as IR.S.V
import Hasura.Backends.DataWrapper.IR.Table as IR.T
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
  type BackendConfig 'DataWrapper = Adapter.DataConnectorBackendConfig
  type SourceConfig 'DataWrapper = Adapter.SourceConfig
  type SourceConnConfiguration 'DataWrapper = Adapter.ConnSourceConfig

  type TableName 'DataWrapper = IR.T.Name
  type FunctionName 'DataWrapper = IR.F.Name
  type RawFunctionInfo 'DataWrapper = XDisable
  type FunctionArgType 'DataWrapper = XDisable
  type ConstraintName 'DataWrapper = Unimplemented
  type BasicOrderType 'DataWrapper = IR.O.OrderType
  type NullsOrderType 'DataWrapper = Unimplemented
  type CountType 'DataWrapper = Unimplemented
  type Column 'DataWrapper = IR.C.Name
  type ScalarValue 'DataWrapper = IR.S.V.Value
  type ScalarType 'DataWrapper = IR.S.T.Type
  type SQLExpression 'DataWrapper = IR.E.Expression
  type SQLOperator 'DataWrapper = IR.E.Operator
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
  isNumType IR.S.T.Number = True
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
    G.mkName (IR.N.unName name)
      `onNothing` throw400 ValidationFailed ("TableName " <> IR.N.unName name <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'DataWrapper -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: not implemented for GraphQL Data Wrappers."

  scalarTypeGraphQLName :: ScalarType 'DataWrapper -> Either QErr G.Name
  scalarTypeGraphQLName = \case
    IR.S.T.String -> pure stringScalar
    IR.S.T.Number -> pure floatScalar
    IR.S.T.Bool -> pure boolScalar

  snakeCaseTableName :: TableName 'DataWrapper -> Text
  snakeCaseTableName = IR.N.unName
