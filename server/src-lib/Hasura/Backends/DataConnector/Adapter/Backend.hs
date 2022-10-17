{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Backend (CustomBooleanOperator (..)) where

import Data.Aeson qualified as J
import Data.Aeson.Extended (ToJSONKeyValue (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types qualified as J
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Casing qualified as C
import Data.Text.Extended ((<<>))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error (Code (ValidationFailed), QErr, runAesonParser, throw400)
import Hasura.Incremental
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend (Backend (..), ComputedFieldReturnType, SupportedNamingCase (..), XDisable, XEnable)
import Hasura.SQL.Backend (BackendType (DataConnector))
import Hasura.Server.Types (ServerReplicas)
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
  type BackendConfig 'DataConnector = InsOrdHashMap DC.DataConnectorName DC.DataConnectorOptions
  type BackendInfo 'DataConnector = HashMap DC.DataConnectorName DC.DataConnectorInfo
  type SourceConfig 'DataConnector = DC.SourceConfig
  type SourceConnConfiguration 'DataConnector = DC.ConnSourceConfig

  type TableName 'DataConnector = DC.TableName
  type FunctionName 'DataConnector = DC.FunctionName
  type RawFunctionInfo 'DataConnector = XDisable
  type FunctionArgument 'DataConnector = XDisable
  type ConstraintName 'DataConnector = DC.ConstraintName
  type BasicOrderType 'DataConnector = DC.OrderDirection
  type NullsOrderType 'DataConnector = Unimplemented
  type CountType 'DataConnector = DC.CountAggregate
  type Column 'DataConnector = DC.ColumnName
  type ScalarValue 'DataConnector = J.Value
  type ScalarType 'DataConnector = DC.ScalarType

  -- This does not actually have to be the full IR Expression, in fact it is only
  -- required to represent literals, so we use a special type for that.
  -- The 'SQLExpression' type family should be removed in a future refactor
  type SQLExpression 'DataConnector = DC.Literal
  type ScalarSelectionArguments 'DataConnector = Void
  type BooleanOperators 'DataConnector = CustomBooleanOperator
  type ExtraTableMetadata 'DataConnector = Unimplemented
  type ComputedFieldDefinition 'DataConnector = Unimplemented
  type FunctionArgumentExp 'DataConnector = Const Unimplemented
  type ComputedFieldImplicitArguments 'DataConnector = Unimplemented
  type ComputedFieldReturn 'DataConnector = Unimplemented

  type XComputedField 'DataConnector = XDisable
  type XRelay 'DataConnector = XDisable
  type XNodesAgg 'DataConnector = XEnable
  type XNestedInserts 'DataConnector = XDisable
  type XStreamingSubscription 'DataConnector = XDisable

  type HealthCheckTest 'DataConnector = Void

  isComparableType :: ScalarType 'DataConnector -> Bool
  isComparableType = \case
    DC.NumberTy -> True
    DC.StringTy -> True
    DC.BoolTy -> False
    DC.CustomTy _ -> False -- TODO: extend Capabilities for custom types

  isNumType :: ScalarType 'DataConnector -> Bool
  isNumType DC.NumberTy = True
  isNumType _ = False

  textToScalarValue :: Maybe Text -> ScalarValue 'DataConnector
  textToScalarValue = error "textToScalarValue: not implemented for the Data Connector backend."

  parseScalarValue :: ScalarType 'DataConnector -> J.Value -> Either QErr (ScalarValue 'DataConnector)
  parseScalarValue type' value = runAesonParser (parseValue type') value

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
  tableGraphQLName name = do
    let snakedName = snakeCaseTableName @'DataConnector name
    G.mkName snakedName
      `onNothing` throw400 ValidationFailed ("TableName " <> snakedName <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'DataConnector -> Either QErr G.Name
  functionGraphQLName = error "functionGraphQLName: not implemented for the Data Connector backend."

  snakeCaseTableName :: TableName 'DataConnector -> Text
  snakeCaseTableName = Text.intercalate "_" . NonEmpty.toList . DC.unTableName

  getTableIdentifier :: TableName 'DataConnector -> Either QErr C.GQLNameIdentifier
  getTableIdentifier name@(DC.TableName (prefix :| suffixes)) =
    let identifier = do
          namePrefix <- G.mkName prefix
          nameSuffixes <- traverse G.mkNameSuffix suffixes
          pure $ C.fromAutogeneratedTuple (namePrefix, nameSuffixes)
     in identifier
          `onNothing` throw400 ValidationFailed ("TableName " <> name <<> " is not a valid GraphQL identifier")

  namingConventionSupport :: SupportedNamingCase
  namingConventionSupport = OnlyHasuraCase

  resizeSourcePools :: SourceConfig 'DataConnector -> ServerReplicas -> IO ()
  resizeSourcePools _sourceConfig _serverReplicas =
    -- Data connectors do not have concept of connection pools
    pure ()

data CustomBooleanOperator a = CustomBooleanOperator
  { _cboName :: Text,
    _cboRHS :: Maybe (Either (RootOrCurrentColumn 'DataConnector) a) -- TODO turn Either into a specific type
  }
  deriving stock (Eq, Generic, Foldable, Functor, Traversable, Show)

instance NFData a => NFData (CustomBooleanOperator a)

instance Hashable a => Hashable (CustomBooleanOperator a)

instance Cacheable a => Cacheable (CustomBooleanOperator a)

instance J.ToJSON a => ToJSONKeyValue (CustomBooleanOperator a) where
  toJSONKeyValue CustomBooleanOperator {..} = (fromText _cboName, J.toJSON _cboRHS)

parseValue :: DC.ScalarType -> J.Value -> J.Parser J.Value
parseValue type' val =
  case (type', val) of
    (_, J.Null) -> pure J.Null
    (DC.StringTy, value) -> J.String <$> J.parseJSON value
    (DC.BoolTy, value) -> J.Bool <$> J.parseJSON value
    (DC.NumberTy, value) -> J.Number <$> J.parseJSON value
    -- For custom scalar types we don't know what subset of JSON values
    -- they accept, so we just accept any value.
    (DC.CustomTy _, value) -> pure value
