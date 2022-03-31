{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.Agent.SchemaSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataWrapper.API.V0.TableSpec (genTableInfo)
import Hasura.Backends.DataWrapper.Agent.Schema
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "Capabilities" $ do
    testToFromJSONToSchema (Capabilities False) [aesonQQ|{"relationships": false}|]
    jsonOpenApiProperties genCapabilities
  describe "SchemaResponse" $ do
    testToFromJSONToSchema (SchemaResponse (Capabilities True) []) [aesonQQ|{"capabilities": {"relationships": true}, "tables": []}|]
    jsonOpenApiProperties genSchemaResponse
  describe "QueryResponse" $ do
    testToFromJSONToSchema (QueryResponse []) [aesonQQ|[]|]
    jsonOpenApiProperties genQueryResponse

genCapabilities :: MonadGen m => m Capabilities
genCapabilities = Capabilities <$> Gen.bool

genSchemaResponse :: MonadGen m => m SchemaResponse
genSchemaResponse =
  SchemaResponse
    <$> genCapabilities
    <*> Gen.list (linear 0 5) genTableInfo

genQueryResponse :: MonadGen m => m QueryResponse
genQueryResponse =
  QueryResponse <$> Gen.list (linear 0 5) genObject
