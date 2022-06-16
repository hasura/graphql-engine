{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Capabilities
import Hasura.Backends.DataConnector.API.V0.ConfigSchema
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
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

genQueryCapabilities :: MonadGen m => m QueryCapabilities
genQueryCapabilities = QueryCapabilities <$> Gen.bool

genMutationCapabilities :: MonadGen m => m MutationCapabilities
genMutationCapabilities = pure MutationCapabilities {}

genSubscriptionCapabilities :: MonadGen m => m SubscriptionCapabilities
genSubscriptionCapabilities = pure SubscriptionCapabilities {}

genBooleanOperators :: MonadGen m => m BooleanOperators
genBooleanOperators = pure BooleanOperators {}

genComparisonOperators :: MonadGen m => m ComparisonOperators
genComparisonOperators = pure ComparisonOperators {}

genFilteringCapabilities :: MonadGen m => m FilteringCapabilities
genFilteringCapabilities =
  FilteringCapabilities
    <$> genBooleanOperators
    <*> genComparisonOperators

genRelationshipCapabilities :: MonadGen m => m RelationshipCapabilities
genRelationshipCapabilities = pure RelationshipCapabilities {}

genCapabilities :: MonadGen m => m Capabilities
genCapabilities =
  Capabilities
    <$> Gen.maybe genQueryCapabilities
    <*> Gen.maybe genMutationCapabilities
    <*> Gen.maybe genSubscriptionCapabilities
    <*> Gen.maybe genFilteringCapabilities
    <*> Gen.maybe genRelationshipCapabilities

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
