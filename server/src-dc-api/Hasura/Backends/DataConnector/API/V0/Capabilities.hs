{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use onNothing" #-}

module Hasura.Backends.DataConnector.API.V0.Capabilities
  ( Capabilities (..),
    QueryCapabilities (..),
    MutationCapabilities (..),
    SubscriptionCapabilities (..),
    ScalarTypeCapabilities (..),
    ScalarTypesCapabilities (..),
    GraphQLTypeDefinitions,
    RelationshipCapabilities (..),
    ComparisonCapabilities (..),
    CrossTableComparisonCapabilities (..),
    MetricsCapabilities (..),
    ExplainCapabilities (..),
    CapabilitiesResponse (..),
    emptyCapabilities,
    lookupComparisonInputObjectDefinition,
    mkGraphQLTypeDefinitions,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
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
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject), Schema (..), ToSchema (..), declareSchemaRef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as Builder
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (ConfigSchemaResponse)
import Language.GraphQL.Draft.Parser qualified as GQL.Parser
import Language.GraphQL.Draft.Printer qualified as GQL.Printer
import Language.GraphQL.Draft.Syntax qualified as GQL.Syntax
import Prelude

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { cQueries :: Maybe QueryCapabilities,
    cMutations :: Maybe MutationCapabilities,
    cSubscriptions :: Maybe SubscriptionCapabilities,
    cScalarTypes :: Maybe ScalarTypesCapabilities,
    cGraphQLTypeDefinitions :: Maybe GraphQLTypeDefinitions,
    cRelationships :: Maybe RelationshipCapabilities,
    cComparisons :: Maybe ComparisonCapabilities,
    cMetrics :: Maybe MetricsCapabilities,
    cExplain :: Maybe ExplainCapabilities
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

emptyCapabilities :: Capabilities
emptyCapabilities = Capabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities
        <$> optionalField "queries" "The agent's query capabilities" .= cQueries
        <*> optionalField "mutations" "The agent's mutation capabilities" .= cMutations
        <*> optionalField "subscriptions" "The agent's subscription capabilities" .= cSubscriptions
        <*> optionalField "scalarTypes" "The agent's scalar types and their capabilities" .= cScalarTypes
        <*> optionalField "graphqlSchema" "A GraphQL Schema Document describing the agent's scalar types and input object types for comparison operators" .= cGraphQLTypeDefinitions
        <*> optionalField "relationships" "The agent's relationship capabilities" .= cRelationships
        <*> optionalField "comparisons" "The agent's comparison capabilities" .= cComparisons
        <*> optionalField "metrics" "The agent's metrics capabilities" .= cMetrics
        <*> optionalField "explain" "The agent's explain capabilities" .= cExplain

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

nameCodec :: JSONCodec GQL.Syntax.Name
nameCodec =
  bimapCodec
    parseName
    GQL.Syntax.unName
    (StringCodec (Just "GraphQLName"))
    <?> "A valid GraphQL name"
  where
    parseName text = maybe (Left $ Text.unpack text <> " is not a valid GraphQL name") pure $ GQL.Syntax.mkName text

data ScalarTypeCapabilities = ScalarTypeCapabilities
  { stcComparisonInputObject :: Maybe GQL.Syntax.Name
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypeCapabilities

instance HasCodec ScalarTypeCapabilities where
  codec =
    object
      "ScalarTypeCapabilities"
      ( ScalarTypeCapabilities
          <$> optionalFieldWith' "comparisonType" nameCodec .= stcComparisonInputObject
      )
      <??> [ "Capabilities of a scalar type.",
             "comparisonType: Name of the GraphQL input object to be used for comparison operations on the scalar type. The input object type must be defined in the `graphqlSchema`."
           ]

newtype ScalarTypesCapabilities = ScalarTypesCapabilities
  { unScalarTypesCapabilities :: HashMap GQL.Syntax.Name ScalarTypeCapabilities
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarTypesCapabilities

instance HasCodec ScalarTypesCapabilities where
  codec =
    named "ScalarTypesCapabilities" $
      dimapCodec ScalarTypesCapabilities unScalarTypesCapabilities (hashMapCodec codec)
        <??> [ "A map from scalar type names to their capabilities.",
               "Keys must be valid GraphQL names and must be defined as scalar types in the `graphqlSchema`"
             ]

type TypeDefinition = GQL.Syntax.TypeDefinition () GQL.Syntax.InputValueDefinition

mkGraphQLTypeDefinitions :: NonEmpty TypeDefinition -> GraphQLTypeDefinitions
mkGraphQLTypeDefinitions =
  GraphQLTypeDefinitions
    . InsOrdHashMap.fromList
    . toList
    . fmap (\td -> (getName td, td))
  where
    getName :: TypeDefinition -> GQL.Syntax.Name
    getName = \case
      GQL.Syntax.TypeDefinitionScalar GQL.Syntax.ScalarTypeDefinition {..} -> _stdName
      GQL.Syntax.TypeDefinitionObject GQL.Syntax.ObjectTypeDefinition {..} -> _otdName
      GQL.Syntax.TypeDefinitionInterface GQL.Syntax.InterfaceTypeDefinition {..} -> _itdName
      GQL.Syntax.TypeDefinitionUnion GQL.Syntax.UnionTypeDefinition {..} -> _utdName
      GQL.Syntax.TypeDefinitionEnum GQL.Syntax.EnumTypeDefinition {..} -> _etdName
      GQL.Syntax.TypeDefinitionInputObject GQL.Syntax.InputObjectTypeDefinition {..} -> _iotdName

newtype GraphQLTypeDefinitions = GraphQLTypeDefinitions
  { gtdTypeDefinitions :: InsOrdHashMap GQL.Syntax.Name TypeDefinition
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec GraphQLTypeDefinitions

instance HasCodec GraphQLTypeDefinitions where
  codec =
    bimapCodec parseTypeDefinitions printTypeDefinitions (StringCodec (Just "GraphQLTypeDefinitions"))
      <?> "A valid GraphQL schema document containing type definitions"
    where
      -- Note: any `SchemaDefinition`s in the parsed `SchemaDocument` will be ignored.
      -- We don't need them, we're only interested in the `TypeDefinition`s defined in the document.
      getTypeDefinition :: GQL.Syntax.TypeSystemDefinition -> Maybe TypeDefinition
      getTypeDefinition = \case
        GQL.Syntax.TypeSystemDefinitionSchema _ -> Nothing
        GQL.Syntax.TypeSystemDefinitionType td -> Just td

      fromSchemaDocument :: GQL.Syntax.SchemaDocument -> Either String GraphQLTypeDefinitions
      fromSchemaDocument (GQL.Syntax.SchemaDocument typeSystemDefinitions) =
        case nonEmpty $ mapMaybe getTypeDefinition typeSystemDefinitions of
          Nothing -> Left "No type definitions found in schema document"
          Just typeDefinitions -> Right $ mkGraphQLTypeDefinitions typeDefinitions

      parseTypeDefinitions :: Text -> Either String GraphQLTypeDefinitions
      parseTypeDefinitions =
        fromSchemaDocument <=< first Text.unpack . GQL.Parser.parseSchemaDocument

      printTypeDefinitions :: GraphQLTypeDefinitions -> Text
      printTypeDefinitions =
        toStrict
          . Builder.toLazyText
          . GQL.Printer.schemaDocument
          . GQL.Syntax.SchemaDocument
          . fmap GQL.Syntax.TypeSystemDefinitionType
          . toList
          . gtdTypeDefinitions

data ComparisonCapabilities = ComparisonCapabilities
  {_ccCrossTableComparisonCapabilities :: Maybe CrossTableComparisonCapabilities}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonCapabilities

instance HasCodec ComparisonCapabilities where
  codec =
    object "ComparisonCapabilities" $
      ComparisonCapabilities
        <$> optionalFieldOrNull "cross_table" "The agent supports comparisons that involve tables other than the one being queried" .= _ccCrossTableComparisonCapabilities

data CrossTableComparisonCapabilities = CrossTableComparisonCapabilities
  {_ctccSupportsRelations :: Bool}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec CrossTableComparisonCapabilities

instance HasCodec CrossTableComparisonCapabilities where
  codec =
    object "CrossTableComparisonCapabilities" $
      CrossTableComparisonCapabilities
        <$> requiredField "supports_relations" "Does the agent support comparisons that involve related tables (ie. joins)?" .= _ctccSupportsRelations

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

data CapabilitiesResponse = CapabilitiesResponse
  { crCapabilities :: Capabilities,
    crConfigSchemaResponse :: ConfigSchemaResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Autodocodec CapabilitiesResponse

instance HasCodec CapabilitiesResponse where
  codec =
    object "CapabilitiesResponse" $
      CapabilitiesResponse
        <$> requiredField "capabilities" "The capabilities of the agent" .= crCapabilities
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

lookupComparisonInputObjectDefinition :: Capabilities -> GQL.Syntax.Name -> Maybe (GQL.Syntax.InputObjectTypeDefinition GQL.Syntax.InputValueDefinition)
lookupComparisonInputObjectDefinition Capabilities {..} typeName = do
  scalarTypesMap <- cScalarTypes
  ScalarTypeCapabilities {..} <- HashMap.lookup typeName $ unScalarTypesCapabilities scalarTypesMap
  comparisonTypeName <- stcComparisonInputObject
  typeDefinitions <- cGraphQLTypeDefinitions
  typeDefinition <- InsOrdHashMap.lookup comparisonTypeName $ gtdTypeDefinitions typeDefinitions
  case typeDefinition of
    GQL.Syntax.TypeDefinitionInputObject inputObjectTypeDefinition -> Just inputObjectTypeDefinition
    _ -> Nothing
