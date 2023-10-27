{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.SchemaSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Function
import Hasura.Backends.DataConnector.API.V0.FunctionSpec (genFunctionName)
import Hasura.Backends.DataConnector.API.V0.Schema
import Hasura.Backends.DataConnector.API.V0.Table
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableInfo, genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "SchemaRequest" $ do
    describe "Minimal" $ do
      testToFromJSONToSchema
        (SchemaRequest mempty Everything)
        [aesonQQ| {} |]
    describe "Full" $ do
      testToFromJSONToSchema
        (SchemaRequest (SchemaFilters (Just [TableName ["my_table"]]) (Just [FunctionName ["my_function"]])) BasicInfo)
        [aesonQQ|
          { "filters": {
              "only_tables": [["my_table"]],
              "only_functions": [["my_function"]]
            },
            "detail_level": "basic_info"
          }
        |]
    jsonOpenApiProperties genSchemaRequest

  describe "SchemaResponse" $ do
    testToFromJSONToSchema (SchemaResponse [] [] Nothing) [aesonQQ|{"tables": []}|]
    jsonOpenApiProperties genSchemaResponse

genSchemaRequest :: (MonadGen m) => m SchemaRequest
genSchemaRequest =
  SchemaRequest
    <$> genSchemaFilters
    <*> genDetailLevel

genSchemaFilters :: (MonadGen m) => m SchemaFilters
genSchemaFilters =
  SchemaFilters
    <$> Gen.maybe (Gen.list defaultRange genTableName)
    <*> Gen.maybe (Gen.list defaultRange genFunctionName)

genDetailLevel :: (MonadGen m) => m DetailLevel
genDetailLevel =
  Gen.element [Everything, BasicInfo]

genSchemaResponse :: Gen SchemaResponse
genSchemaResponse = do
  tables <- Gen.list defaultRange genTableInfo
  pure $ SchemaResponse tables [] Nothing
