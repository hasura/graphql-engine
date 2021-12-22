{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.Backend () where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J (Value)
import Hasura.Base.Error (Code (ValidationFailed), QErr, throw400)
import Hasura.Experimental.IR.Column qualified as Column (Name)
import Hasura.Experimental.IR.Expression (Expression, Operator)
import Hasura.Experimental.IR.Function qualified as Function (Name)
import Hasura.Experimental.IR.Name as Name (Name (unName))
import Hasura.Experimental.IR.OrderBy (OrderType)
import Hasura.Experimental.IR.Scalar.Type qualified as Scalar (Type)
import Hasura.Experimental.IR.Scalar.Type qualified as Scalar.Type (Type (..))
import Hasura.Experimental.IR.Scalar.Value qualified as Scalar (Value)
import Hasura.Experimental.IR.Table as Table (Name)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..), XDisable)
import Hasura.RQL.Types.Common as RQL (boolScalar, floatScalar, stringScalar)
import Hasura.SQL.Backend (BackendType (Experimental))
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------

-- | An alias for '()' indicating that a particular associated type has not yet
-- been implemented for the 'Experimental' backend.
--
-- '()' is used (rather than a type with an empty data constructor) because it
-- comes with many of the instances that these associated types require.
--
-- This alias should /not/ be exported from this module, and it's only defined
-- for clarity.
type Unimplemented = ()

instance Backend 'Experimental where
  type SourceConfig 'Experimental = Unimplemented
  type SourceConnConfiguration 'Experimental = Unimplemented

  type TableName 'Experimental = Table.Name
  type FunctionName 'Experimental = Function.Name
  type RawFunctionInfo 'Experimental = XDisable
  type FunctionArgType 'Experimental = XDisable
  type ConstraintName 'Experimental = Unimplemented
  type BasicOrderType 'Experimental = OrderType
  type NullsOrderType 'Experimental = Unimplemented
  type CountType 'Experimental = Unimplemented
  type Column 'Experimental = Column.Name
  type ScalarValue 'Experimental = Scalar.Value
  type ScalarType 'Experimental = Scalar.Type
  type SQLExpression 'Experimental = Expression
  type SQLOperator 'Experimental = Operator
  type BooleanOperators 'Experimental = Const XDisable
  type ExtraTableMetadata 'Experimental = Unimplemented

  type XComputedField 'Experimental = XDisable
  type XRelay 'Experimental = XDisable
  type XNodesAgg 'Experimental = XDisable
  type XNestedInserts 'Experimental = XDisable

  functionArgScalarType :: FunctionArgType 'Experimental -> ScalarType 'Experimental
  functionArgScalarType = error "functionArgScalarType: not implemented yet"

  isComparableType :: ScalarType 'Experimental -> Bool
  isComparableType = isNumType @'Experimental

  isNumType :: ScalarType 'Experimental -> Bool
  isNumType Scalar.Type.Number = True
  isNumType _ = False

  textToScalarValue :: Maybe Text -> ScalarValue 'Experimental
  textToScalarValue = error "textToScalarValue: Experimental backend does not support this operation yet."

  parseScalarValue :: ScalarType 'Experimental -> J.Value -> Either QErr (ScalarValue 'Experimental)
  parseScalarValue = error "parseScalarValue: Experimental backend does not support this operation yet."

  scalarValueToJSON :: ScalarValue 'Experimental -> J.Value
  scalarValueToJSON = error "scalarValueToJSON: Experimental backend does not support this operation yet."

  functionToTable :: FunctionName 'Experimental -> TableName 'Experimental
  functionToTable = error "functionToTable: Experimental backend does not support this operation yet."

  -- phil said this was cursed
  tableToFunction :: TableName 'Experimental -> FunctionName 'Experimental
  tableToFunction = coerce

  tableGraphQLName :: TableName 'Experimental -> Either QErr G.Name
  tableGraphQLName name =
    G.mkName (Name.unName name)
      `onNothing` throw400 ValidationFailed ("TableName " <> Name.unName name <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'Experimental -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: Experimental backend does not support this operation yet."

  scalarTypeGraphQLName :: ScalarType 'Experimental -> Either QErr G.Name
  scalarTypeGraphQLName = \case
    Scalar.Type.String -> pure stringScalar
    Scalar.Type.Number -> pure floatScalar
    Scalar.Type.Bool -> pure boolScalar

  snakeCaseTableName :: TableName 'Experimental -> Text
  snakeCaseTableName = Name.unName
