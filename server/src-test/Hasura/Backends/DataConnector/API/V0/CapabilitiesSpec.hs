{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.RawString (raw)
import Hasura.Backends.DataConnector.API.V0.Capabilities
import Hasura.Backends.DataConnector.API.V0.ConfigSchema
import Hasura.Backends.DataConnector.API.V0.Scalar (ScalarType (..))
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
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
    testToFromJSONToSchema defaultCapabilities [aesonQQ|{}|]
    jsonOpenApiProperties genCapabilities
  describe "CapabilitiesResponse" $ do
    testToFromJSON
      (CapabilitiesResponse (defaultCapabilities {_cRelationships = Just RelationshipCapabilities {}}) emptyConfigSchemaResponse)
      [aesonQQ|{"capabilities": {"relationships": {}}, "config_schemas": {"config_schema": {}, "other_schemas": {}}}|]
  describe "ScalarTypesCapabilities" $ do
    testToFromJSONToSchema (ScalarTypesCapabilities (HashMap.singleton StringTy (ScalarTypeCapabilities Nothing Nothing))) [aesonQQ|{"string": {}}|]
    jsonOpenApiProperties genScalarTypesCapabilities
  describe "ScalarTypeCapabilities" $ do
    testToFromJSONToSchema (ScalarTypeCapabilities (Just [G.name|DateTimeComparisons|]) Nothing) [aesonQQ|{"comparison_type": "DateTimeComparisons"}|]
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

genDataSchemaCapabilities :: MonadGen m => m DataSchemaCapabilities
genDataSchemaCapabilities =
  DataSchemaCapabilities
    <$> Gen.bool
    <*> Gen.bool
    <*> genColumnNullability

genColumnNullability :: MonadGen m => m ColumnNullability
genColumnNullability =
  Gen.element [NullableAndNonNullableColumns, OnlyNullableColumns]

genQueryCapabilities :: MonadGen m => m QueryCapabilities
genQueryCapabilities = pure QueryCapabilities

genMutationCapabilities :: MonadGen m => m MutationCapabilities
genMutationCapabilities = pure MutationCapabilities {}

genSubscriptionCapabilities :: MonadGen m => m SubscriptionCapabilities
genSubscriptionCapabilities = pure SubscriptionCapabilities {}

genAggregateFunctions :: MonadGen m => m AggregateFunctions
genAggregateFunctions =
  AggregateFunctions <$> genHashMap (genGName defaultRange) genScalarType defaultRange

genScalarTypeCapabilities :: MonadGen m => m ScalarTypeCapabilities
genScalarTypeCapabilities =
  ScalarTypeCapabilities
    <$> Gen.maybe (genGName defaultRange)
    <*> Gen.maybe genAggregateFunctions

genScalarTypesCapabilities :: MonadGen m => m ScalarTypesCapabilities
genScalarTypesCapabilities =
  ScalarTypesCapabilities <$> genHashMap genScalarType genScalarTypeCapabilities defaultRange

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

genComparisonCapabilities :: MonadGen m => m ComparisonCapabilities
genComparisonCapabilities =
  ComparisonCapabilities
    <$> Gen.maybe genSubqueryComparisonCapabilities

genSubqueryComparisonCapabilities :: MonadGen m => m SubqueryComparisonCapabilities
genSubqueryComparisonCapabilities =
  SubqueryComparisonCapabilities
    <$> Gen.bool

genMetricsCapabilities :: MonadGen m => m MetricsCapabilities
genMetricsCapabilities = pure MetricsCapabilities {}

genExplainCapabilities :: MonadGen m => m ExplainCapabilities
genExplainCapabilities = pure ExplainCapabilities {}

genRawCapabilities :: MonadGen m => m RawCapabilities
genRawCapabilities = pure RawCapabilities {}

genCapabilities :: Gen Capabilities
genCapabilities =
  Capabilities
    <$> genDataSchemaCapabilities
    <*> Gen.maybe genQueryCapabilities
    <*> Gen.maybe genMutationCapabilities
    <*> Gen.maybe genSubscriptionCapabilities
    <*> Gen.maybe genScalarTypesCapabilities
    <*> Gen.maybe genGraphQLTypeDefinitions
    <*> Gen.maybe genRelationshipCapabilities
    <*> Gen.maybe genComparisonCapabilities
    <*> Gen.maybe genMetricsCapabilities
    <*> Gen.maybe genExplainCapabilities
    <*> Gen.maybe genRawCapabilities

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
