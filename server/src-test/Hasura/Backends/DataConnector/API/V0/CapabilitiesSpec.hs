{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec
  ( spec,
    genUpdateColumnOperatorName,
  )
where

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
      (CapabilitiesResponse (defaultCapabilities {_cRelationships = Just RelationshipCapabilities {}}) emptyConfigSchemaResponse Nothing Nothing)
      [aesonQQ|{"capabilities": {"relationships": {}}, "config_schemas": {"config_schema": {}, "other_schemas": {}}}|]
  describe "ScalarTypesCapabilities" $ do
    describe "Minimal"
      $ testToFromJSONToSchema (ScalarTypesCapabilities (HashMap.singleton (ScalarType "string") (ScalarTypeCapabilities mempty mempty mempty Nothing))) [aesonQQ|{"string": {}}|]
    describe "Maximal" $ do
      let comparisonOperators = ComparisonOperators $ HashMap.fromList [([G.name|same_day_as|], ScalarType "DateTime")]
      let aggregateFunctions = AggregateFunctions $ HashMap.fromList [([G.name|max|], ScalarType "DateTime")]
      let updateColumnOperators = UpdateColumnOperators $ HashMap.fromList [(UpdateColumnOperatorName [G.name|set_year|], UpdateColumnOperatorDefinition (ScalarType "Number"))]
      let graphQLType = Just GraphQLString
      let json =
            [aesonQQ|{
              "comparison_operators": {
                "same_day_as": "DateTime"
              },
              "aggregate_functions": {
                "max": "DateTime"
              },
              "update_column_operators": {
                "set_year": {
                  "argument_type": "Number"
                }
              },
              "graphql_type": "String"
            }|]
      testToFromJSONToSchema (ScalarTypeCapabilities comparisonOperators aggregateFunctions updateColumnOperators graphQLType) json
    jsonOpenApiProperties genScalarTypesCapabilities

genDataSchemaCapabilities :: (MonadGen m) => m DataSchemaCapabilities
genDataSchemaCapabilities =
  DataSchemaCapabilities
    <$> Gen.bool
    <*> Gen.bool
    <*> genColumnNullability
    <*> Gen.bool

genPostSchemaCapabilities :: (MonadGen m) => m PostSchemaCapabilities
genPostSchemaCapabilities = pure PostSchemaCapabilities {}

genColumnNullability :: (MonadGen m) => m ColumnNullability
genColumnNullability =
  Gen.element [NullableAndNonNullableColumns, OnlyNullableColumns]

genQueryCapabilities :: (MonadGen m) => m QueryCapabilities
genQueryCapabilities = QueryCapabilities <$> Gen.maybe genForeachCapabilities <*> Gen.maybe genRedactionCapabilities

genForeachCapabilities :: (MonadGen m) => m ForeachCapabilities
genForeachCapabilities = pure ForeachCapabilities

genRedactionCapabilities :: (MonadGen m) => m RedactionCapabilities
genRedactionCapabilities = pure RedactionCapabilities

genMutationCapabilities :: (MonadGen m) => m MutationCapabilities
genMutationCapabilities =
  MutationCapabilities
    <$> Gen.maybe genInsertCapabilities
    <*> Gen.maybe genUpdateCapabilities
    <*> Gen.maybe genDeleteCapabilities
    <*> Gen.maybe genAtomicitySupportLevel
    <*> Gen.maybe genReturningCapabilities

genInsertCapabilities :: (MonadGen m) => m InsertCapabilities
genInsertCapabilities = InsertCapabilities <$> Gen.bool

genUpdateCapabilities :: (MonadGen m) => m UpdateCapabilities
genUpdateCapabilities = pure UpdateCapabilities

genDeleteCapabilities :: (MonadGen m) => m DeleteCapabilities
genDeleteCapabilities = pure DeleteCapabilities

genAtomicitySupportLevel :: (MonadGen m) => m AtomicitySupportLevel
genAtomicitySupportLevel = Gen.enumBounded

genReturningCapabilities :: (MonadGen m) => m ReturningCapabilities
genReturningCapabilities = pure ReturningCapabilities

genSubscriptionCapabilities :: (MonadGen m) => m SubscriptionCapabilities
genSubscriptionCapabilities = pure SubscriptionCapabilities {}

genComparisonOperators :: (MonadGen m, GenBase m ~ Identity) => m ComparisonOperators
genComparisonOperators =
  ComparisonOperators <$> genHashMap (genGName defaultRange) genScalarType defaultRange

genAggregateFunctions :: (MonadGen m, GenBase m ~ Identity) => m AggregateFunctions
genAggregateFunctions =
  AggregateFunctions <$> genHashMap (genGName defaultRange) genScalarType defaultRange

genGraphQLType :: (MonadGen m) => m GraphQLType
genGraphQLType = Gen.enumBounded

genScalarTypeCapabilities :: (MonadGen m, GenBase m ~ Identity) => m ScalarTypeCapabilities
genScalarTypeCapabilities =
  ScalarTypeCapabilities
    <$> genComparisonOperators
    <*> genAggregateFunctions
    <*> genUpdateColumnOperators
    <*> Gen.maybe genGraphQLType

genScalarTypesCapabilities :: (MonadGen m, GenBase m ~ Identity) => m ScalarTypesCapabilities
genScalarTypesCapabilities =
  ScalarTypesCapabilities <$> genHashMap genScalarType genScalarTypeCapabilities defaultRange

genUpdateColumnOperators :: (MonadGen m, GenBase m ~ Identity) => m UpdateColumnOperators
genUpdateColumnOperators =
  UpdateColumnOperators <$> genHashMap genUpdateColumnOperatorName genUpdateColumnOperatorDefinition defaultRange

genUpdateColumnOperatorName :: (MonadGen m) => m UpdateColumnOperatorName
genUpdateColumnOperatorName = UpdateColumnOperatorName <$> genGName defaultRange

genUpdateColumnOperatorDefinition :: (MonadGen m, GenBase m ~ Identity) => m UpdateColumnOperatorDefinition
genUpdateColumnOperatorDefinition = UpdateColumnOperatorDefinition <$> genScalarType

genRelationshipCapabilities :: (MonadGen m) => m RelationshipCapabilities
genRelationshipCapabilities = pure RelationshipCapabilities {}

genInterpolatedQueryCapabilities :: (MonadGen m) => m InterpolatedQueryCapabilities
genInterpolatedQueryCapabilities = pure InterpolatedQueryCapabilities {}

genComparisonCapabilities :: (MonadGen m) => m ComparisonCapabilities
genComparisonCapabilities =
  ComparisonCapabilities
    <$> Gen.maybe genSubqueryComparisonCapabilities

genSubqueryComparisonCapabilities :: (MonadGen m) => m SubqueryComparisonCapabilities
genSubqueryComparisonCapabilities =
  SubqueryComparisonCapabilities
    <$> Gen.bool

genMetricsCapabilities :: (MonadGen m) => m MetricsCapabilities
genMetricsCapabilities = pure MetricsCapabilities {}

genExplainCapabilities :: (MonadGen m) => m ExplainCapabilities
genExplainCapabilities = pure ExplainCapabilities {}

genRawCapabilities :: (MonadGen m) => m RawCapabilities
genRawCapabilities = pure RawCapabilities {}

genDatasetCapabilities :: (MonadGen m) => m DatasetCapabilities
genDatasetCapabilities = pure DatasetCapabilities {}

genUserDefinedFunctionCapabilities :: (MonadGen m) => m UserDefinedFunctionCapabilities
genUserDefinedFunctionCapabilities = pure UserDefinedFunctionCapabilities {}

genLicensing :: (MonadGen m) => m Licensing
genLicensing = pure Licensing {}

genCapabilities :: Gen Capabilities
genCapabilities =
  Capabilities
    <$> genDataSchemaCapabilities
    <*> Gen.maybe genPostSchemaCapabilities
    <*> Gen.maybe genQueryCapabilities
    <*> Gen.maybe genMutationCapabilities
    <*> Gen.maybe genSubscriptionCapabilities
    <*> genScalarTypesCapabilities
    <*> Gen.maybe genRelationshipCapabilities
    <*> Gen.maybe genInterpolatedQueryCapabilities
    <*> Gen.maybe genComparisonCapabilities
    <*> Gen.maybe genMetricsCapabilities
    <*> Gen.maybe genExplainCapabilities
    <*> Gen.maybe genRawCapabilities
    <*> Gen.maybe genDatasetCapabilities
    <*> Gen.maybe genUserDefinedFunctionCapabilities
    <*> Gen.maybe genLicensing

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
