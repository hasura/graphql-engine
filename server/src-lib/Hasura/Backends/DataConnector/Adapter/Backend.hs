{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Backend
  ( CustomBooleanOperator (..),
    columnTypeToScalarType,
    parseValue,
    lookupGraphQLType,
  )
where

import Control.Lens (view)
import Data.Aeson qualified as J
import Data.Aeson.Extended (ToJSONKeyValue (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as Text
import Data.Text.Casing qualified as C
import Data.Text.Extended ((<<>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Adapter.Types.Mutations qualified as DC
import Hasura.Base.Error (Code (ValidationFailed), QErr, runAesonParser, throw400, throw500)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..), ComputedFieldReturnType, HasSourceConfiguration (..), SupportedNamingCase (..), XDisable, XEnable)
import Hasura.RQL.Types.BackendType (BackendSourceKind (DataConnectorKind), BackendType (DataConnector))
import Hasura.RQL.Types.Column (ColumnType (..))
import Hasura.RQL.Types.ResizePool
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

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
  type BackendConfig 'DataConnector = Map DC.DataConnectorName DC.DataConnectorOptions
  type BackendInfo 'DataConnector = HashMap DC.DataConnectorName DC.DataConnectorInfo

  type TableName 'DataConnector = DC.TableName
  type FunctionName 'DataConnector = DC.FunctionName
  type FunctionReturnType 'DataConnector = DC.FunctionReturnType
  type RawFunctionInfo 'DataConnector = API.FunctionInfo
  type FunctionArgument 'DataConnector = API.FunctionArg
  type ConstraintName 'DataConnector = DC.ConstraintName
  type BasicOrderType 'DataConnector = DC.OrderDirection
  type NullsOrderType 'DataConnector = Unimplemented
  type CountType 'DataConnector = DC.CountAggregate
  type Column 'DataConnector = DC.ColumnName
  type ColumnPath 'DataConnector = DC.ColumnPath
  type ScalarValue 'DataConnector = J.Value
  type ScalarType 'DataConnector = DC.ScalarType

  -- This does not actually have to be the full IR Expression, in fact it is only
  -- required to represent literals, so we use a special type for that.
  -- The 'SQLExpression' type family should be removed in a future refactor
  type SQLExpression 'DataConnector = DC.Literal
  type ScalarSelectionArguments 'DataConnector = Void
  type BooleanOperators 'DataConnector = CustomBooleanOperator
  type ExtraTableMetadata 'DataConnector = DC.ExtraTableMetadata
  type ComputedFieldDefinition 'DataConnector = Unimplemented
  type FunctionArgumentExp 'DataConnector = DC.ArgumentExp
  type ComputedFieldImplicitArguments 'DataConnector = Unimplemented
  type ComputedFieldReturn 'DataConnector = Unimplemented

  type UpdateVariant 'DataConnector = DC.DataConnectorUpdateVariant
  type BackendInsert 'DataConnector = DC.BackendInsert

  type XComputedField 'DataConnector = XDisable
  type XRelay 'DataConnector = XDisable
  type XNodesAgg 'DataConnector = XEnable
  type XEventTriggers 'DataConnector = XDisable
  type XNestedInserts 'DataConnector = XDisable
  type XStreamingSubscription 'DataConnector = XDisable
  type XNestedObjects 'DataConnector = XEnable

  type HealthCheckTest 'DataConnector = Void

  isComparableType :: ScalarType 'DataConnector -> Bool
  isComparableType = const False

  isNumType :: ScalarType 'DataConnector -> Bool
  isNumType = const False

  getCustomAggregateOperators :: DC.SourceConfig -> HashMap G.Name (HashMap DC.ScalarType DC.ScalarType)
  getCustomAggregateOperators DC.SourceConfig {..} =
    HashMap.foldrWithKey insertOps mempty scalarTypesCapabilities
    where
      scalarTypesCapabilities = API.unScalarTypesCapabilities $ API._cScalarTypes _scCapabilities
      insertOps typeName API.ScalarTypeCapabilities {..} m =
        HashMap.foldrWithKey insertOp m
          $ API.unAggregateFunctions _stcAggregateFunctions
        where
          insertOp funtionName resultTypeName =
            HashMap.insertWith HashMap.union funtionName
              $ HashMap.singleton
                (Witch.from typeName)
                (Witch.from resultTypeName)

  textToScalarValue :: Maybe Text -> ScalarValue 'DataConnector
  textToScalarValue = error "textToScalarValue: not implemented for the Data Connector backend."

  parseScalarValue :: API.ScalarTypesCapabilities -> ScalarType 'DataConnector -> J.Value -> Either QErr (ScalarValue 'DataConnector)
  parseScalarValue ctx type' value = runAesonParser (parseValue ctx type') value

  scalarValueToJSON :: ScalarValue 'DataConnector -> J.Value
  scalarValueToJSON = id

  -- TODO: Fill in this definition for computed fields
  computedFieldFunction :: ComputedFieldDefinition 'DataConnector -> FunctionName 'DataConnector
  computedFieldFunction = error "computedFieldFunction: not implemented for the Data Connector backend"

  computedFieldReturnType :: ComputedFieldReturn 'DataConnector -> ComputedFieldReturnType 'DataConnector
  computedFieldReturnType = error "computedFieldReturnType: not implemented for the Data Connector backend"

  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments 'DataConnector -> [FunctionArgumentExp 'DataConnector v]
  fromComputedFieldImplicitArguments = error "fromComputedFieldImplicitArguments: not implemented for the Data Connector backend"

  -- phil said this was cursed
  tableToFunction :: TableName 'DataConnector -> FunctionName 'DataConnector
  tableToFunction = coerce

  functionToTable :: FunctionName 'DataConnector -> TableName 'DataConnector
  functionToTable = coerce

  tableGraphQLName :: TableName 'DataConnector -> Either QErr G.Name
  tableGraphQLName name = do
    let snakedName = snakeCaseTableName @'DataConnector name
    G.mkName snakedName
      `onNothing` throw400 ValidationFailed ("TableName " <> snakedName <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'DataConnector -> Either QErr G.Name
  functionGraphQLName name = do
    let snakedName = snakeCaseTableName @'DataConnector (coerce name)
    G.mkName snakedName
      `onNothing` throw400 ValidationFailed ("FunctionName " <> snakedName <> " is not a valid GraphQL name")

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

  resizeSourcePools :: SourceConfig 'DataConnector -> ServerReplicas -> IO SourceResizePoolSummary
  resizeSourcePools _sourceConfig _serverReplicas =
    -- Data connectors do not have concept of connection pools
    pure noPoolsResizedSummary

  defaultTriggerOnReplication = Nothing

  getColVals _ _ _ _ _ _ = throw500 "getColVals: not implemented for the Data Connector backend"

  getColumnPathColumn = \case
    DC.CPPath p -> NonEmpty.head p
    DC.CPColumn c -> c

  tryColumnPathToColumn = \case
    DC.CPPath (column :| []) -> Just column
    DC.CPColumn column -> Just column
    _ -> Nothing

  backendSupportsNestedObjects = pure ()

  sourceSupportsSchemalessTables =
    view $ DC.scCapabilities . API.cDataSchema . API.dscSupportsSchemalessTables

instance HasSourceConfiguration 'DataConnector where
  type SourceConfig 'DataConnector = DC.SourceConfig
  type SourceConnConfiguration 'DataConnector = DC.ConnSourceConfig
  type ScalarTypeParsingContext 'DataConnector = API.ScalarTypesCapabilities

  sourceConfigNumReadReplicas = const 0 -- not supported
  sourceConfigConnectonTemplateEnabled = const False -- not supported
  sourceSupportsColumnRedaction DC.SourceConfig {..} =
    _scCapabilities & API._cQueries >>= API._qcRedaction & isJust
  sourceConfigBackendSourceKind DC.SourceConfig {..} = DataConnectorKind _scDataConnectorName

data CustomBooleanOperator a = CustomBooleanOperator
  { _cboName :: Text,
    _cboRHS :: Maybe a
  }
  deriving stock (Eq, Generic, Foldable, Functor, Traversable, Show)

instance (NFData a) => NFData (CustomBooleanOperator a)

instance (Hashable a) => Hashable (CustomBooleanOperator a)

instance (J.ToJSON a) => ToJSONKeyValue (CustomBooleanOperator a) where
  toJSONKeyValue CustomBooleanOperator {..} = (fromText _cboName, J.toJSON _cboRHS)

parseValue :: API.ScalarTypesCapabilities -> DC.ScalarType -> J.Value -> J.Parser J.Value
parseValue ctx type' val =
  case val of
    J.Null -> pure J.Null
    value -> case lookupGraphQLType ctx type' of
      Nothing -> pure value
      Just DC.GraphQLInt -> (J.Number . fromIntegral) <$> J.parseJSON @Int value
      Just DC.GraphQLFloat -> (J.Number . fromFloatDigits) <$> J.parseJSON @Double value
      Just DC.GraphQLString -> J.String <$> J.parseJSON value
      Just DC.GraphQLBoolean -> J.Bool <$> J.parseJSON value
      Just DC.GraphQLID -> J.String <$> parseID value
  where
    parseID value = J.parseJSON @Text value <|> tshow <$> J.parseJSON @Int value

lookupGraphQLType :: API.ScalarTypesCapabilities -> DC.ScalarType -> Maybe DC.GraphQLType
lookupGraphQLType capabilities type' =
  API._stcGraphQLType =<< HashMap.lookup (Witch.from type') (API.unScalarTypesCapabilities capabilities)

columnTypeToScalarType :: ColumnType 'DataConnector -> DC.ScalarType
columnTypeToScalarType = \case
  ColumnScalar scalarType -> scalarType
  -- Data connectors does not yet support enum tables.
  -- If/when we add this support, we probably want to
  -- embed the enum scalar type name within the `EnumReference` record type
  ColumnEnumReference _ -> error "columnTypeToScalarType got enum"
