{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text.RawString (raw)
import Hasura.Backends.DataConnector.API.V0.Capabilities
import Hasura.Backends.DataConnector.API.V0.ConfigSchema
import Hasura.Generator.Common
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Generator (genTypeDefinition)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "Capabilities" $ do
    testToFromJSONToSchema emptyCapabilities [aesonQQ|{}|]
    jsonOpenApiProperties genCapabilities
  describe "CapabilitiesResponse" $ do
    testToFromJSON
      (CapabilitiesResponse (emptyCapabilities {cRelationships = Just RelationshipCapabilities {}}) emptyConfigSchemaResponse)
      [aesonQQ|{"capabilities": {"relationships": {}}, "configSchemas": {"configSchema": {}, "otherSchemas": {}}}|]
  describe "ScalarTypeCapabilities" $ do
    testToFromJSONToSchema (ScalarTypeCapabilities $ Just [G.name|DateTimeComparisons|]) [aesonQQ|{"comparisonType": "DateTimeComparisons"}|]
  describe "GraphQLTypeDefinitions" $ do
    testToFromJSONToSchema sampleGraphQLTypeDefinitions sampleGraphQLTypeDefinitionsJSON

sampleGraphQLTypeDefinitions :: GraphQLTypeDefinitions
sampleGraphQLTypeDefinitions =
  mkGraphQLTypeDefinitions
    [ G.TypeDefinitionScalar $ G.ScalarTypeDefinition Nothing [G.name|DateTime|] [],
      G.TypeDefinitionInputObject $
        G.InputObjectTypeDefinition
          Nothing
          [G.name|DateTimeComparisons|]
          []
          [ G.InputValueDefinition Nothing [G.name|after|] (G.TypeNamed (G.Nullability True) [G.name|DateTime|]) Nothing [],
            G.InputValueDefinition Nothing [G.name|before|] (G.TypeNamed (G.Nullability True) [G.name|DateTime|]) Nothing [],
            G.InputValueDefinition Nothing [G.name|in_year|] (G.TypeNamed (G.Nullability True) [G.name|Int|]) Nothing []
          ]
    ]

sampleGraphQLTypeDefinitionsJSON :: Value
sampleGraphQLTypeDefinitionsJSON =
  [raw|scalar DateTime

input DateTimeComparisons {after: DateTime
  before: DateTime
  in_year: Int
}|]

genQueryCapabilities :: MonadGen m => m QueryCapabilities
genQueryCapabilities = QueryCapabilities <$> Gen.bool

genMutationCapabilities :: MonadGen m => m MutationCapabilities
genMutationCapabilities = pure MutationCapabilities {}

genSubscriptionCapabilities :: MonadGen m => m SubscriptionCapabilities
genSubscriptionCapabilities = pure SubscriptionCapabilities {}

genScalarTypeCapabilities :: MonadGen m => m ScalarTypeCapabilities
genScalarTypeCapabilities = ScalarTypeCapabilities <$> Gen.maybe (genGName defaultRange)

genScalarTypesCapabilities :: MonadGen m => m ScalarTypesCapabilities
genScalarTypesCapabilities =
  ScalarTypesCapabilities <$> genHashMap (genGName defaultRange) genScalarTypeCapabilities defaultRange

-- | 'genTypeDefinition' generates invalid type definitions so we need to filter them out.
-- The printers also sort various lists upon printing, so we need to pre-sort them for round-tripping to work.
-- The printer for 'ObjectTypeDefinition' prints directives in the wrong place so we only allow
-- definitions with no directives.
-- TODO: fix this in `graphql-parser-hs`.
isValidTypeDefinition :: Ord inputType => G.TypeDefinition possibleTypes inputType -> Maybe (G.TypeDefinition possibleTypes inputType)
isValidTypeDefinition = \case
  t@(G.TypeDefinitionScalar G.ScalarTypeDefinition {}) -> Just t
  G.TypeDefinitionObject G.ObjectTypeDefinition {..} -> do
    guard $ not $ null _otdFieldsDefinition
    Just $
      G.TypeDefinitionObject
        G.ObjectTypeDefinition
          { _otdFieldsDefinition = sort _otdFieldsDefinition,
            _otdDirectives = [],
            ..
          }
  G.TypeDefinitionInterface G.InterfaceTypeDefinition {..} -> do
    guard $ not $ null _itdFieldsDefinition
    Just $
      G.TypeDefinitionInterface
        G.InterfaceTypeDefinition {_itdFieldsDefinition = sort _itdFieldsDefinition, ..}
  G.TypeDefinitionUnion G.UnionTypeDefinition {..} -> do
    guard $ not $ null _utdMemberTypes
    Just $
      G.TypeDefinitionUnion
        G.UnionTypeDefinition {_utdMemberTypes = sort _utdMemberTypes, ..}
  G.TypeDefinitionEnum G.EnumTypeDefinition {..} -> do
    guard $ not $ null _etdValueDefinitions
    Just $
      G.TypeDefinitionEnum
        G.EnumTypeDefinition {_etdValueDefinitions = sort _etdValueDefinitions, ..}
  G.TypeDefinitionInputObject G.InputObjectTypeDefinition {..} -> do
    guard $ not $ null _iotdValueDefinitions
    Just $
      G.TypeDefinitionInputObject
        G.InputObjectTypeDefinition {_iotdValueDefinitions = sort _iotdValueDefinitions, ..}

genGraphQLTypeDefinitions :: Gen GraphQLTypeDefinitions
genGraphQLTypeDefinitions =
  mkGraphQLTypeDefinitions <$> Gen.nonEmpty defaultRange (Gen.mapMaybe isValidTypeDefinition genTypeDefinition)

genRelationshipCapabilities :: MonadGen m => m RelationshipCapabilities
genRelationshipCapabilities = pure RelationshipCapabilities {}

genMetricsCapabilities :: MonadGen m => m MetricsCapabilities
genMetricsCapabilities = pure MetricsCapabilities {}

genExplainCapabilities :: MonadGen m => m ExplainCapabilities
genExplainCapabilities = pure ExplainCapabilities {}

genCapabilities :: Gen Capabilities
genCapabilities =
  Capabilities
    <$> Gen.maybe genQueryCapabilities
    <*> Gen.maybe genMutationCapabilities
    <*> Gen.maybe genSubscriptionCapabilities
    <*> Gen.maybe genScalarTypesCapabilities
    <*> Gen.maybe genGraphQLTypeDefinitions
    <*> Gen.maybe genRelationshipCapabilities
    <*> Gen.maybe genMetricsCapabilities
    <*> Gen.maybe genExplainCapabilities

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
