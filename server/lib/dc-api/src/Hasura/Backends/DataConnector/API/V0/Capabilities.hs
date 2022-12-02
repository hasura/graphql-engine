{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use onNothing" #-}

module Hasura.Backends.DataConnector.API.V0.Capabilities
  ( Capabilities (..),
    defaultCapabilities,
    DataSchemaCapabilities (..),
    defaultDataSchemaCapabilities,
    ColumnNullability (..),
    QueryCapabilities (..),
    MutationCapabilities (..),
    InsertCapabilities (..),
    UpdateCapabilities (..),
    DeleteCapabilities (..),
    AtomicitySupportLevel (..),
    ReturningCapabilities (..),
    SubscriptionCapabilities (..),
    ComparisonOperators (..),
    AggregateFunctions (..),
    GraphQLType (..),
    ScalarTypeCapabilities (..),
    ScalarTypesCapabilities (..),
    RelationshipCapabilities (..),
    ComparisonCapabilities (..),
    SubqueryComparisonCapabilities (..),
    MetricsCapabilities (..),
    ExplainCapabilities (..),
    RawCapabilities (..),
    CapabilitiesResponse (..),
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Data (Data, Proxy (..))
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Monoid (Last (..))
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject, OpenApiString), Referenced (..), Schema (..), ToSchema (..), declareSchemaRef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as Builder
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (ConfigSchemaResponse)
import Hasura.Backends.DataConnector.API.V0.Name (nameCodec)
import Hasura.Backends.DataConnector.API.V0.Scalar (ScalarType (..))
import Language.GraphQL.Draft.Parser qualified as GQL.Parser
import Language.GraphQL.Draft.Printer qualified as GQL.Printer
import Language.GraphQL.Draft.Syntax qualified as GQL.Syntax
import Servant.API (HasStatus)
import Servant.API.UVerb qualified as Servant
import Prelude

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { _cDataSchema :: DataSchemaCapabilities,
    _cQueries :: Maybe QueryCapabilities,
    _cMutations :: Maybe MutationCapabilities,
    _cSubscriptions :: Maybe SubscriptionCapabilities,
    _cScalarTypes :: ScalarTypesCapabilities,
    _cRelationships :: Maybe RelationshipCapabilities,
    _cComparisons :: Maybe ComparisonCapabilities,
    _cMetrics :: Maybe MetricsCapabilities,
    _cExplain :: Maybe ExplainCapabilities,
    _cRaw :: Maybe RawCapabilities
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities defaultDataSchemaCapabilities Nothing Nothing Nothing mempty Nothing Nothing Nothing Nothing Nothing

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities
        <$> optionalFieldWithOmittedDefault "data_schema" defaultDataSchemaCapabilities "The agent's data schema capabilities" .= _cDataSchema
        <*> optionalField "queries" "The agent's query capabilities" .= _cQueries
        <*> optionalField "mutations" "The agent's mutation capabilities" .= _cMutations
        <*> optionalField "subscriptions" "The agent's subscription capabilities" .= _cSubscriptions
        <*> optionalFieldWithOmittedDefault "scalar_types" mempty "The agent's scalar types and their capabilities" .= _cScalarTypes
        <*> optionalField "relationships" "The agent's relationship capabilities" .= _cRelationships
        <*> optionalField "comparisons" "The agent's comparison capabilities" .= _cComparisons
        <*> optionalField "metrics" "The agent's metrics capabilities" .= _cMetrics
        <*> optionalField "explain" "The agent's explain capabilities" .= _cExplain
        <*> optionalField "raw" "The agent's raw query capabilities" .= _cRaw

data DataSchemaCapabilities = DataSchemaCapabilities
  { _dscSupportsPrimaryKeys :: Bool,
    _dscSupportsForeignKeys :: Bool,
    _dscColumnNullability :: ColumnNullability
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DataSchemaCapabilities

defaultDataSchemaCapabilities :: DataSchemaCapabilities
defaultDataSchemaCapabilities =
  DataSchemaCapabilities False False NullableAndNonNullableColumns

instance HasCodec DataSchemaCapabilities where
  codec =
    object "DataSchemaCapabilities" $
      DataSchemaCapabilities
        <$> optionalFieldWithOmittedDefault "supports_primary_keys" (_dscSupportsPrimaryKeys defaultDataSchemaCapabilities) "Whether tables can have primary keys" .= _dscSupportsPrimaryKeys
        <*> optionalFieldWithOmittedDefault "supports_foreign_keys" (_dscSupportsForeignKeys defaultDataSchemaCapabilities) "Whether tables can have foreign keys" .= _dscSupportsForeignKeys
        <*> optionalFieldWithOmittedDefault "column_nullability" (_dscColumnNullability defaultDataSchemaCapabilities) "The sort of column nullability that is supported" .= _dscColumnNullability

data ColumnNullability
  = OnlyNullableColumns
  | NullableAndNonNullableColumns
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnNullability

instance HasCodec ColumnNullability where
  codec =
    named "ColumnNullability" $
      stringConstCodec
        [ (OnlyNullableColumns, "only_nullable"),
          (NullableAndNonNullableColumns, "nullable_and_non_nullable")
        ]

data QueryCapabilities = QueryCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec QueryCapabilities

instance HasCodec QueryCapabilities where
  codec =
    object "QueryCapabilities" $ pure QueryCapabilities

data MutationCapabilities = MutationCapabilities
  { _mcInsertCapabilities :: Maybe InsertCapabilities,
    _mcUpdateCapabilities :: Maybe UpdateCapabilities,
    _mcDeleteCapabilities :: Maybe DeleteCapabilities,
    _mcAtomicitySupportLevel :: Maybe AtomicitySupportLevel,
    _mcReturningCapabilities :: Maybe ReturningCapabilities
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationCapabilities

instance HasCodec MutationCapabilities where
  codec =
    object "MutationCapabilities" $
      MutationCapabilities
        <$> optionalField "insert" "Whether or not the agent supports insert mutations" .= _mcInsertCapabilities
        <*> optionalField "update" "Whether or not the agent supports update mutations" .= _mcUpdateCapabilities
        <*> optionalField "delete" "Whether or not the agent supports delete mutations" .= _mcDeleteCapabilities
        <*> optionalField "atomicity_support_level" "What level of transactional atomicity does the agent support for mutations" .= _mcAtomicitySupportLevel
        <*> optionalField "returning" "Whether or not the agent supports returning the mutation-affected rows" .= _mcReturningCapabilities

data InsertCapabilities = InsertCapabilities
  { _icSupportsNestedInserts :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec InsertCapabilities

instance HasCodec InsertCapabilities where
  codec =
    object "InsertCapabilities" $
      InsertCapabilities
        <$> optionalFieldWithDefault "supports_nested_inserts" False "Whether or not nested inserts to related tables are supported" .= _icSupportsNestedInserts

data UpdateCapabilities = UpdateCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec UpdateCapabilities

instance HasCodec UpdateCapabilities where
  codec =
    object "UpdateCapabilities" $ pure UpdateCapabilities

data DeleteCapabilities = DeleteCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DeleteCapabilities

instance HasCodec DeleteCapabilities where
  codec =
    object "DeleteCapabilities" $ pure DeleteCapabilities

data AtomicitySupportLevel
  = RowAtomicity
  | SingleOperationAtomicity
  | HomogeneousOperationsAtomicity
  | HeterogeneousOperationsAtomicity
  deriving stock (Eq, Ord, Show, Generic, Data, Enum, Bounded)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec AtomicitySupportLevel

instance HasCodec AtomicitySupportLevel where
  codec =
    named "AtomicitySupportLevel" $
      stringConstCodec
        [ (RowAtomicity, "row"),
          (SingleOperationAtomicity, "single_operation"),
          (HomogeneousOperationsAtomicity, "homogeneous_operations"),
          (HeterogeneousOperationsAtomicity, "heterogeneous_operations")
        ]
        <??> [ "Describes the level of transactional atomicity the agent supports for mutation operations.",
               "'row': If multiple rows are affected in a single operation but one fails, only the failed row's changes will be reverted",
               "'single_operation': If multiple rows are affected in a single operation but one fails, all affected rows in the operation will be reverted",
               "'homogeneous_operations': If multiple operations of only the same type exist in the one mutation request, a failure in one will result in all changes being reverted",
               "'heterogeneous_operations': If multiple operations of any type exist in the one mutation request, a failure in one will result in all changes being reverted"
             ]

data ReturningCapabilities = ReturningCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ReturningCapabilities

instance HasCodec ReturningCapabilities where
  codec =
    object "ReturningCapabilities" $ pure ReturningCapabilities

data SubscriptionCapabilities = SubscriptionCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SubscriptionCapabilities

instance HasCodec SubscriptionCapabilities where
  codec = object "SubscriptionCapabilities" $ pure SubscriptionCapabilities

data RelationshipCapabilities = RelationshipCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RelationshipCapabilities

instance HasCodec RelationshipCapabilities where
  codec = object "RelationshipCapabilities" $ pure RelationshipCapabilities

newtype ComparisonOperators = ComparisonOperators
  { unComparisonOperators :: HashMap GQL.Syntax.Name ScalarType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonOperators

instance HasCodec ComparisonOperators where
  codec =
    named "ComparisonOperators" $
      dimapCodec ComparisonOperators unComparisonOperators (hashMapCodec codec)
        <??> [ "A map from comparison operator names to their argument types.",
               "Operator and argument type names must be valid GraphQL names.",
               "Result type names must be defined scalar types - either built-in or declared in ScalarTypesCapabilities."
             ]

newtype AggregateFunctions = AggregateFunctions
  { unAggregateFunctions :: HashMap GQL.Syntax.Name ScalarType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec AggregateFunctions

instance HasCodec AggregateFunctions where
  codec =
    named "AggregateFunctions" $
      dimapCodec AggregateFunctions unAggregateFunctions (hashMapCodec codec)
        <??> [ "A map from aggregate function names to their result types.",
               "Function and result type names must be valid GraphQL names.",
               "Result type names must be defined scalar types - either built-in or declared in ScalarTypesCapabilities."
             ]

data GraphQLType
  = GraphQLInt
  | GraphQLFloat
  | GraphQLString
  | GraphQLBoolean
  | GraphQLID
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec GraphQLType

instance HasCodec GraphQLType where
  codec =
    named "GraphQLType" $
      stringConstCodec
        [ (GraphQLInt, "Int"),
          (GraphQLFloat, "Float"),
          (GraphQLString, "String"),
          (GraphQLBoolean, "Boolean"),
          (GraphQLID, "ID")
        ]

data ScalarTypeCapabilities = ScalarTypeCapabilities
  { _stcComparisonOperators :: ComparisonOperators,
    _stcAggregateFunctions :: AggregateFunctions,
    _stcGraphQLType :: Maybe GraphQLType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypeCapabilities

instance Semigroup ScalarTypeCapabilities where
  a <> b =
    ScalarTypeCapabilities
      { _stcComparisonOperators = _stcComparisonOperators a <> _stcComparisonOperators b,
        _stcAggregateFunctions = _stcAggregateFunctions a <> _stcAggregateFunctions b,
        _stcGraphQLType = _stcGraphQLType b <|> _stcGraphQLType a
      }

instance Monoid ScalarTypeCapabilities where
  mempty = ScalarTypeCapabilities mempty mempty Nothing

instance HasCodec ScalarTypeCapabilities where
  codec =
    object
      "ScalarTypeCapabilities"
      ( ScalarTypeCapabilities
          <$> optionalFieldWithOmittedDefault' "comparison_operators" mempty
            .= _stcComparisonOperators
          <*> optionalFieldWithOmittedDefault' "aggregate_functions" mempty
            .= _stcAggregateFunctions
          <*> optionalField' "graphql_type"
            .= _stcGraphQLType
      )
      <??> [ "Capabilities of a scalar type.",
             "comparison_operators: The comparison operators supported by the scalar type.",
             "aggregate_functions: The aggregate functions supported by the scalar type.",
             "graphql_type: Associates the custom scalar type with one of the built-in GraphQL scalar types.  If a `graphql_type` is specified then HGE will use the parser for that built-in type when parsing values of the custom type. If not given then any JSON value will be accepted."
           ]

newtype ScalarTypesCapabilities = ScalarTypesCapabilities
  { unScalarTypesCapabilities :: HashMap ScalarType ScalarTypeCapabilities
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypesCapabilities

instance Semigroup ScalarTypesCapabilities where
  ScalarTypesCapabilities a <> ScalarTypesCapabilities b = ScalarTypesCapabilities $ HashMap.unionWith (<>) a b

instance Monoid ScalarTypesCapabilities where
  mempty = ScalarTypesCapabilities mempty

instance HasCodec ScalarTypesCapabilities where
  codec =
    named "ScalarTypesCapabilities" $
      dimapCodec ScalarTypesCapabilities unScalarTypesCapabilities (hashMapCodec codec)
        <??> [ "A map from scalar type names to their capabilities.",
               "Keys must be valid GraphQL names and must be defined as scalar types in the `graphql_schema`"
             ]

data ComparisonCapabilities = ComparisonCapabilities
  {_ccSubqueryComparisonCapabilities :: Maybe SubqueryComparisonCapabilities}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonCapabilities

instance HasCodec ComparisonCapabilities where
  codec =
    object "ComparisonCapabilities" $
      ComparisonCapabilities
        <$> optionalFieldOrNull "subquery" "The agent supports comparisons that involve tables other than the one being queried"
          .= _ccSubqueryComparisonCapabilities

data SubqueryComparisonCapabilities = SubqueryComparisonCapabilities
  {_ctccSupportsRelations :: Bool}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SubqueryComparisonCapabilities

instance HasCodec SubqueryComparisonCapabilities where
  codec =
    object "SubqueryComparisonCapabilities" $
      SubqueryComparisonCapabilities
        <$> optionalFieldWithOmittedDefault "supports_relations" False "Does the agent support comparisons that involve related tables (ie. joins)?"
          .= _ctccSupportsRelations

data MetricsCapabilities = MetricsCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MetricsCapabilities

instance HasCodec MetricsCapabilities where
  codec =
    object "MetricsCapabilities" $ pure MetricsCapabilities

data ExplainCapabilities = ExplainCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ExplainCapabilities

instance HasCodec ExplainCapabilities where
  codec =
    object "ExplainCapabilities" $ pure ExplainCapabilities

data RawCapabilities = RawCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RawCapabilities

instance HasCodec RawCapabilities where
  codec =
    object "RawCapabilities" $ pure RawCapabilities

data CapabilitiesResponse = CapabilitiesResponse
  { _crCapabilities :: Capabilities,
    _crConfigSchemaResponse :: ConfigSchemaResponse,
    _crDisplayName :: Maybe Text,
    _crReleaseName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Autodocodec CapabilitiesResponse

instance Servant.HasStatus CapabilitiesResponse where
  type StatusOf CapabilitiesResponse = 200

instance HasCodec CapabilitiesResponse where
  codec =
    object "CapabilitiesResponse" $
      CapabilitiesResponse
        <$> requiredField "capabilities" "The capabilities of the agent" .= _crCapabilities
        <*> requiredField "config_schemas" "The agent's configuration schemas" .= _crConfigSchemaResponse
        <*> optionalField "display_name" "The agent's preferred display name" .= _crDisplayName
        <*> optionalField "release_name" "The agent's release name. For example: 'beta'" .= _crDisplayName

instance ToSchema CapabilitiesResponse where
  declareNamedSchema _ = do
    capabilitiesSchemaRef <- declareSchemaRef (Proxy @Capabilities)
    configSchemasSchemaRef <- declareSchemaRef (Proxy @ConfigSchemaResponse)
    let schema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaRequired = ["capabilities", "config_schemas"],
              _schemaProperties =
                InsOrdHashMap.fromList
                  [ ("display_name", Inline (mempty {_schemaType = Just OpenApiString})), -- TODO: Can we derive this from Codec?
                    ("release_name", Inline (mempty {_schemaType = Just OpenApiString})), -- TODO: Can we derive this from Codec?
                    ("capabilities", capabilitiesSchemaRef),
                    ("config_schemas", configSchemasSchemaRef)
                  ]
            }

    pure $ NamedSchema (Just "CapabilitiesResponse") schema
