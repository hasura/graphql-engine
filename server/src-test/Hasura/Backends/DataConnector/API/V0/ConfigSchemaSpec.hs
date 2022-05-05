{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ConfigSchemaSpec (spec) where

import Data.Aeson (toJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Data (Proxy (..))
import Data.OpenApi (OpenApiItems (..), OpenApiType (..), Reference (..), Referenced (..), Schema (..), toSchema)
import Hasura.Backends.DataConnector.API.V0.API
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

    it "produces the correct OpenAPI Spec once external schema refs are fixed up" $
      fixExternalSchemaRefsInSchema (toJSON $ toSchema (Proxy @ConfigSchemaResponse))
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
              "$ref": "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/80c781e479f85ac67001ceb3e7e410e25d2a561b/schemas/v3.0/schema.json#/definitions/Schema"
            },
            "otherSchemas": {
              "additionalProperties": {
                "$ref": "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/80c781e479f85ac67001ceb3e7e410e25d2a561b/schemas/v3.0/schema.json#/definitions/Schema"
              },
              "type": "object",
              "nullable": false
            }
          }
        }
      |]
