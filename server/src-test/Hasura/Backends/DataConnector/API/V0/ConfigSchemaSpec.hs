{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ConfigSchemaSpec (spec) where

import Data.Aeson (toJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Data (Proxy (..))
import Data.OpenApi (OpenApiItems (..), OpenApiType (..), Reference (..), Referenced (..), Schema (..), toSchema)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Prelude
import Test.Aeson.Utils (testToFromJSON)
import Test.Hspec

spec :: Spec
spec = do
  describe "ConfigSchemaResponse" $ do
    let tableNameSchema =
          mempty
            { _schemaType = Just OpenApiString,
              _schemaNullable = Just False
            }
        tablesSchema =
          mempty
            { _schemaDescription = Just "List of tables to make available in the schema and for querying",
              _schemaType = Just OpenApiArray,
              _schemaNullable = Just True,
              _schemaItems = Just $ OpenApiItemsObject (Ref $ Reference "TableName")
            }
        _csrConfigSchema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaProperties =
                [("tables", Ref $ Reference "Tables")]
            }
        _csrOtherSchemas =
          [ ("Tables", tablesSchema),
            ("TableName", tableNameSchema)
          ]
        val = ConfigSchemaResponse {..}
        jsonVal =
          [aesonQQ|
          {
            "config_schema": {
              "type": "object",
              "nullable": false,
              "properties": {
                "tables": { "$ref": "#/other_schemas/Tables" }
              }
            },
            "other_schemas": {
              "Tables": {
                "description": "List of tables to make available in the schema and for querying",
                "type": "array",
                "items": { "$ref": "#/other_schemas/TableName" },
                "nullable": true
              },
              "TableName": {
                "nullable": false,
                "type": "string"
              }
            }
          }
        |]
    testToFromJSON val jsonVal

    it "OpenAPI spec is as expected"
      $ toJSON (toSchema (Proxy @ConfigSchemaResponse))
      `shouldBe` [aesonQQ|
        {
          "required": [
            "config_schema",
            "other_schemas"
          ],
          "type": "object",
          "nullable": false,
          "properties": {
            "config_schema": {
              "$ref": "#/components/schemas/OpenApiSchema"
            },
            "other_schemas": {
              "additionalProperties": {
                "$ref": "#/components/schemas/OpenApiSchema"
              },
              "type": "object",
              "nullable": false
            }
          }
        }
      |]
