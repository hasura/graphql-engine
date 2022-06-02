{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Backend () where

import Data.Aeson qualified as J (Value)
import Data.Text.Casing qualified as C
import Hasura.Backends.DataConnector.Adapter.Types qualified as Adapter
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Function qualified as IR.F
import Hasura.Backends.DataConnector.IR.Name qualified as IR.N
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S.V
import Hasura.Backends.DataConnector.IR.Table as IR.T
import Hasura.Base.Error (Code (ValidationFailed), QErr, runAesonParser, throw400)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..), ComputedFieldReturnType, SupportedNamingCase (..), XDisable)
import Hasura.RQL.Types.Common as RQL (boolScalar, floatScalar, stringScalar)
import Hasura.SQL.Backend (BackendType (DataConnector))
import Language.GraphQL.Draft.Syntax qualified as G

-- | An alias for '()' indicating that a particular associated type has not yet
-- been implemented for the 'DataConnector' backend.
--
-- '()' is used (rather than a type with an empty data constructor) because it
-- comes with many of the instances that these associated types require.
--
-- This alias should /not/ be exported from this module, and it's only defined
-- for clarity.
type Unimplemented = ()

instance Backend 'DataConnector where
  type BackendConfig 'DataConnector = Adapter.DataConnectorBackendConfig
  type SourceConfig 'DataConnector = Adapter.SourceConfig
  type SourceConnConfiguration 'DataConnector = Adapter.ConnSourceConfig

  type TableName 'DataConnector = IR.T.Name
  type FunctionName 'DataConnector = IR.F.Name
  type RawFunctionInfo 'DataConnector = XDisable
  type FunctionArgument 'DataConnector = XDisable
  type ConstraintName 'DataConnector = Unimplemented
  type BasicOrderType 'DataConnector = IR.O.OrderType
  type NullsOrderType 'DataConnector = Unimplemented
  type CountType 'DataConnector = Unimplemented
  type Column 'DataConnector = IR.C.Name
  type ScalarValue 'DataConnector = IR.S.V.Value
  type ScalarType 'DataConnector = IR.S.T.Type

  -- This does not actually have to be the full IR Expression, in fact it is only
  -- required to represent literals, so we use a special type for that.
  -- The 'SQLExpression' type family should be removed in a future refactor
  type SQLExpression 'DataConnector = IR.S.V.Literal
  type ScalarSelectionArguments 'DataConnector = Void
  type BooleanOperators 'DataConnector = Const XDisable
  type ExtraTableMetadata 'DataConnector = Unimplemented
  type ComputedFieldDefinition 'DataConnector = Unimplemented
  type FunctionArgumentExp 'DataConnector = Const Unimplemented
  type ComputedFieldImplicitArguments 'DataConnector = Unimplemented
  type ComputedFieldReturn 'DataConnector = Unimplemented

  type XComputedField 'DataConnector = XDisable
  type XRelay 'DataConnector = XDisable
  type XNodesAgg 'DataConnector = XDisable
  type XNestedInserts 'DataConnector = XDisable
  type XStreamingSubscription 'DataConnector = XDisable

  isComparableType :: ScalarType 'DataConnector -> Bool
  isComparableType = isNumType @'DataConnector

  isNumType :: ScalarType 'DataConnector -> Bool
  isNumType IR.S.T.Number = True
  isNumType _ = False

  textToScalarValue :: Maybe Text -> ScalarValue 'DataConnector
  textToScalarValue = error "textToScalarValue: not implemented for the Data Connector backend."

  parseScalarValue :: ScalarType 'DataConnector -> J.Value -> Either QErr (ScalarValue 'DataConnector)
  parseScalarValue type' value = runAesonParser (IR.S.V.parseValue type') value

  scalarValueToJSON :: ScalarValue 'DataConnector -> J.Value
  scalarValueToJSON = error "scalarValueToJSON: not implemented for the Data Connector backend."

  functionToTable :: FunctionName 'DataConnector -> TableName 'DataConnector
  functionToTable = error "functionToTable: not implemented for the Data Connector backend."

  computedFieldFunction :: ComputedFieldDefinition 'DataConnector -> FunctionName 'DataConnector
  computedFieldFunction = error "computedFieldFunction: not implemented for the Data Connector backend"

  computedFieldReturnType :: ComputedFieldReturn 'DataConnector -> ComputedFieldReturnType 'DataConnector
  computedFieldReturnType = error "computedFieldReturnType: not implemented for the Data Connector backend"

  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments 'DataConnector -> [FunctionArgumentExp 'DataConnector v]
  fromComputedFieldImplicitArguments = error "fromComputedFieldImplicitArguments: not implemented for the Data Connector backend"

  -- phil said this was cursed
  tableToFunction :: TableName 'DataConnector -> FunctionName 'DataConnector
  tableToFunction = coerce

  tableGraphQLName :: TableName 'DataConnector -> Either QErr G.Name
  tableGraphQLName name =
    G.mkName (IR.N.unName name)
      `onNothing` throw400 ValidationFailed ("TableName " <> IR.N.unName name <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'DataConnector -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: not implemented for the Data Connector backend."

  scalarTypeGraphQLName :: ScalarType 'DataConnector -> Either QErr G.Name
  scalarTypeGraphQLName = \case
    IR.S.T.String -> pure stringScalar
    IR.S.T.Number -> pure floatScalar
    IR.S.T.Bool -> pure boolScalar

  snakeCaseTableName :: TableName 'DataConnector -> Text
  snakeCaseTableName = IR.N.unName

  getTableIdentifier :: TableName 'DataConnector -> Either QErr C.GQLNameIdentifier
  getTableIdentifier name = do
    gqlTableName <-
      G.mkName (IR.N.unName name)
        `onNothing` throw400 ValidationFailed ("TableName " <> IR.N.unName name <> " is not a valid GraphQL identifier")
    pure $ C.Identifier gqlTableName []

  namingConventionSupport :: SupportedNamingCase
  namingConventionSupport = OnlyHasuraCase
