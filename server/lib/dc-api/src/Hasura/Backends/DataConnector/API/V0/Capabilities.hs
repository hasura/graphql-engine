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
    SubscriptionCapabilities (..),
    ComparisonOperators (..),
    AggregateFunctions (..),
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

data MutationCapabilities = MutationCapabilities {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationCapabilities

instance HasCodec MutationCapabilities where
  codec = object "MutationCapabilities" $ pure MutationCapabilities

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
               "Result type names must be defined scalar types - either builtin or declared in ScalarTypesCapabilities."
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
               "Result type names must be defined scalar types - either builtin or declared in ScalarTypesCapabilities."
             ]

data ScalarTypeCapabilities = ScalarTypeCapabilities
  { _stcComparisonOperators :: ComparisonOperators,
    _stcAggregateFunctions :: AggregateFunctions
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypeCapabilities

instance HasCodec ScalarTypeCapabilities where
  codec =
    object
      "ScalarTypeCapabilities"
      ( ScalarTypeCapabilities
          <$> optionalFieldWithOmittedDefault' "comparison_operators" mempty .= _stcComparisonOperators
          <*> optionalFieldWithOmittedDefault' "aggregate_functions" mempty .= _stcAggregateFunctions
      )
      <??> [ "Capabilities of a scalar type.",
             "comparison_operators: The comparison operators supported by the scalar type.",
             "aggregate_functions: The aggregate functions supported by the scalar type."
           ]

newtype ScalarTypesCapabilities = ScalarTypesCapabilities
  { unScalarTypesCapabilities :: HashMap ScalarType ScalarTypeCapabilities
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypesCapabilities

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
        <$> optionalFieldOrNull "subquery" "The agent supports comparisons that involve tables other than the one being queried" .= _ccSubqueryComparisonCapabilities

data SubqueryComparisonCapabilities = SubqueryComparisonCapabilities
  {_ctccSupportsRelations :: Bool}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SubqueryComparisonCapabilities

instance HasCodec SubqueryComparisonCapabilities where
  codec =
    object "SubqueryComparisonCapabilities" $
      SubqueryComparisonCapabilities
        <$> optionalFieldWithOmittedDefault "supports_relations" False "Does the agent support comparisons that involve related tables (ie. joins)?" .= _ctccSupportsRelations

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
    _crConfigSchemaResponse :: ConfigSchemaResponse
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
                  [ ("capabilities", capabilitiesSchemaRef),
                    ("config_schemas", configSchemasSchemaRef)
                  ]
            }

    pure $ NamedSchema (Just "CapabilitiesResponse") schema
