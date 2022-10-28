{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0.Capabilities
import Hasura.Backends.DataConnector.API.V0.ConfigSchema
import Hasura.Backends.DataConnector.API.V0.Scalar (ScalarType (..))
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
import Hasura.Generator.Common
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
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
    testToFromJSONToSchema (ScalarTypesCapabilities (HashMap.singleton StringTy (ScalarTypeCapabilities mempty mempty))) [aesonQQ|{"string": {}}|]
    jsonOpenApiProperties genScalarTypesCapabilities
  describe "ScalarTypeCapabilities" $ do
    let comparisonOperators = ComparisonOperators $ HashMap.fromList [([G.name|same_day_as|], CustomTy "DateTime")]
    let aggregateFunctions = AggregateFunctions $ HashMap.fromList [([G.name|max|], CustomTy "DateTime")]
    let json =
          [aesonQQ|{
      "comparison_operators": {
        "same_day_as": "DateTime"
      },
      "aggregate_functions": {
        "max": "DateTime"
      }
    }|]
    testToFromJSONToSchema (ScalarTypeCapabilities comparisonOperators aggregateFunctions) json

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

genComparisonOperators :: MonadGen m => m ComparisonOperators
genComparisonOperators =
  ComparisonOperators <$> genHashMap (genGName defaultRange) genScalarType defaultRange

genAggregateFunctions :: MonadGen m => m AggregateFunctions
genAggregateFunctions =
  AggregateFunctions <$> genHashMap (genGName defaultRange) genScalarType defaultRange

genScalarTypeCapabilities :: MonadGen m => m ScalarTypeCapabilities
genScalarTypeCapabilities =
  ScalarTypeCapabilities
    <$> genComparisonOperators
    <*> genAggregateFunctions

genScalarTypesCapabilities :: MonadGen m => m ScalarTypesCapabilities
genScalarTypesCapabilities =
  ScalarTypesCapabilities <$> genHashMap genScalarType genScalarTypeCapabilities defaultRange

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
    <*> genScalarTypesCapabilities
    <*> Gen.maybe genRelationshipCapabilities
    <*> Gen.maybe genComparisonCapabilities
    <*> Gen.maybe genMetricsCapabilities
    <*> Gen.maybe genExplainCapabilities
    <*> Gen.maybe genRawCapabilities

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
