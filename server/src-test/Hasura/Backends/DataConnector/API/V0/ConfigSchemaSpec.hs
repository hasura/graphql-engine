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
            "configSchema": {
              "type": "object",
              "nullable": false,
              "properties": {
                "tables": { "$ref": "#/otherSchemas/Tables" }
              }
            },
            "otherSchemas": {
              "Tables": {
                "description": "List of tables to make available in the schema and for querying",
                "type": "array",
                "items": { "$ref": "#/otherSchemas/TableName" },
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

    it "OpenAPI spec is as expected" $
      toJSON (toSchema (Proxy @ConfigSchemaResponse))
        `shouldBe` [aesonQQ|
        {
          "required": [
            "configSchema",
            "otherSchemas"
          ],
          "type": "object",
          "nullable": false,
          "properties": {
            "configSchema": {
              "$ref": "#/components/schemas/OpenApiSchema"
            },
            "otherSchemas": {
              "additionalProperties": {
                "$ref": "#/components/schemas/OpenApiSchema"
              },
              "type": "object",
              "nullable": false
            }
          }
        }
      |]
