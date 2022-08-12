{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hasura.Backends.DataConnector.API.V0.Capabilities
  ( Capabilities (..),
    QueryCapabilities (..),
    MutationCapabilities (..),
    SubscriptionCapabilities (..),
    FilteringCapabilities (..),
    BooleanOperators (..),
    ComparisonOperators (..),
    RelationshipCapabilities (..),
    CapabilitiesResponse (..),
    emptyCapabilities,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Proxy (..))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject), Schema (..), ToSchema (..), declareSchemaRef)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (ConfigSchemaResponse)
import Prelude

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { cQueries :: Maybe QueryCapabilities,
    cMutations :: Maybe MutationCapabilities,
    cSubscriptions :: Maybe SubscriptionCapabilities,
    cFiltering :: Maybe FilteringCapabilities,
    cRelationships :: Maybe RelationshipCapabilities
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

emptyCapabilities :: Capabilities
emptyCapabilities = Capabilities Nothing Nothing Nothing Nothing Nothing

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities
        <$> optionalField "queries" "The agent's query capabilities" .= cQueries
        <*> optionalField "mutations" "The agent's mutation capabilities" .= cMutations
        <*> optionalField "subscriptions" "The agent's subscription capabilities" .= cSubscriptions
        <*> optionalField "filtering" "The agent's filtering capabilities" .= cFiltering
        <*> optionalField "relationships" "The agent's relationship capabilities" .= cRelationships

data QueryCapabilities = QueryCapabilities
  { qcSupportsPrimaryKeys :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec QueryCapabilities

instance HasCodec QueryCapabilities where
  codec =
    object "QueryCapabilities" $
      QueryCapabilities
        <$> requiredField "supportsPrimaryKeys" "Does the agent support querying a table by primary key?" .= qcSupportsPrimaryKeys

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

data FilteringCapabilities = FilteringCapabilities
  { fcBooleanOperators :: BooleanOperators,
    fcComparisonOperators :: ComparisonOperators
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FilteringCapabilities

instance HasCodec FilteringCapabilities where
  codec =
    object "FilteringCapabilities" $
      FilteringCapabilities
        <$> requiredField "booleanOperators" "The boolean operators supported by the agent" .= fcBooleanOperators
        <*> requiredField "comparisonOperators" "The comparison operators supported by the agent" .= fcComparisonOperators

data BooleanOperators = BooleanOperators {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec BooleanOperators

instance HasCodec BooleanOperators where
  codec = object "BooleanOperators" $ pure BooleanOperators

data ComparisonOperators = ComparisonOperators {}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonOperators

instance HasCodec ComparisonOperators where
  codec = object "ComparisonOperators" $ pure ComparisonOperators

data CapabilitiesResponse = CapabilitiesResponse
  { crCapabilities :: Capabilities,
    crConfigSchemaResponse :: ConfigSchemaResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Autodocodec CapabilitiesResponse

instance HasCodec CapabilitiesResponse where
  codec =
    object "CapabilitiesResponse" $
      CapabilitiesResponse <$> requiredField "capabilities" "The capabilities of the agent" .= crCapabilities
        <*> requiredField "configSchemas" "The agent's configuration schemas" .= crConfigSchemaResponse

instance ToSchema CapabilitiesResponse where
  declareNamedSchema _ = do
    capabilitiesSchemaRef <- declareSchemaRef (Proxy @Capabilities)
    configSchemasSchemaRef <- declareSchemaRef (Proxy @ConfigSchemaResponse)
    let schema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaRequired = ["capabilities", "configSchemas"],
              _schemaProperties =
                InsOrdHashMap.fromList
                  [ ("capabilities", capabilitiesSchemaRef),
                    ("configSchemas", configSchemasSchemaRef)
                  ]
            }

    pure $ NamedSchema (Just "CapabilitiesResponse") schema
