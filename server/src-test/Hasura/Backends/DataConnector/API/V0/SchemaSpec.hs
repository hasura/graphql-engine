{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.SchemaSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Schema
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableInfo)
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

genCapabilities :: MonadGen m => m Capabilities
genCapabilities = Capabilities <$> Gen.bool

genSchemaResponse :: MonadGen m => m SchemaResponse
genSchemaResponse =
  SchemaResponse
    <$> genCapabilities
    <*> Gen.list (linear 0 5) genTableInfo
