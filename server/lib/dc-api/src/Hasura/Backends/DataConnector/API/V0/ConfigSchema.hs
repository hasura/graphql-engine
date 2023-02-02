{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.DataConnector.API.V0.ConfigSchema
  ( Config (..),
    emptyConfig,
    ConfigSchemaResponse (..),
    validateConfigAgainstConfigSchema,
  )
where

import Autodocodec qualified
import Control.DeepSeq (NFData)
import Control.Lens ((%~), (&), (.~), (^?))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), eitherDecode, encode, object, withObject, (.:), (.=), (<?>))
import Data.Aeson.KeyMap (empty)
import Data.Aeson.Lens (AsValue (..), key, members, values)
import Data.Aeson.Types (JSONPathElement (..))
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Data)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.OpenApi (AdditionalProperties (..), Definitions, NamedSchema (..), OpenApiItems (..), OpenApiType (..), Reference (..), Referenced (..), Schema (..), ToParamSchema (..), ToSchema (..), ValidationError)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (Declare, MonadDeclare (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Prelude

newtype Config = Config {unConfig :: Object}
  deriving stock (Eq, Show, Ord, Data)
  deriving newtype (Hashable, NFData, ToJSON, FromJSON)

emptyConfig :: Config
emptyConfig = Config empty

instance Autodocodec.HasCodec Config where
  codec =
    Autodocodec.named "Config" $
      Autodocodec.dimapCodec Config unConfig Autodocodec.codec

instance FromHttpApiData Config where
  parseUrlPiece = first Text.pack . eitherDecode . BSL.fromStrict . Text.encodeUtf8
  parseHeader = first Text.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData Config where
  toUrlPiece (Config val) = Text.decodeUtf8 . BSL.toStrict $ encode val
  toHeader (Config val) = BSL.toStrict $ encode val

instance ToParamSchema Config where
  toParamSchema _ =
    mempty
      { _schemaType = Just OpenApiObject,
        _schemaNullable = Just False,
        _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
      }

data ConfigSchemaResponse = ConfigSchemaResponse
  { _csrConfigSchema :: Schema,
    _csrOtherSchemas :: Definitions Schema
  }
  deriving stock (Show, Eq)

instance FromJSON ConfigSchemaResponse where
  parseJSON = withObject "ConfigSchemaResponse" $ \obj -> do
    configSchemaValue <- obj .: "config_schema"
    (otherSchemaValues :: Object) <- obj .: "other_schemas"
    _csrConfigSchema <- parseJSON (rewriteConfigSchemaRefsToOpenApiRefs configSchemaValue) <?> Key "config_schema"
    _csrOtherSchemas <- (<?> Key "other_schemas") . parseJSON . Object $ rewriteConfigSchemaRefsToOpenApiRefs <$> otherSchemaValues
    pure ConfigSchemaResponse {..}

instance ToJSON ConfigSchemaResponse where
  toJSON ConfigSchemaResponse {..} =
    let configSchemaValue = rewriteOpenApiRefsToConfigSchemaRefs $ toJSON _csrConfigSchema
        otherSchemasValue = rewriteOpenApiRefsToConfigSchemaRefs . toJSON <$> _csrOtherSchemas
     in object
          [ "config_schema" .= configSchemaValue,
            "other_schemas" .= otherSchemasValue
          ]

instance Autodocodec.HasCodec ConfigSchemaResponse where
  codec = Autodocodec.codecViaAeson "Configuration schemas"

instance ToSchema ConfigSchemaResponse where
  declareNamedSchema _ = do
    openApiSchemaRef <- declareOpenApiSchema
    let otherSchemasSchema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema openApiSchemaRef
            }
    let schema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaRequired = ["config_schema", "other_schemas"],
              _schemaProperties =
                InsOrdHashMap.fromList
                  [ ("config_schema", openApiSchemaRef),
                    ("other_schemas", Inline otherSchemasSchema)
                  ]
            }
    pure $ NamedSchema (Just "ConfigSchemaResponse") schema

-- | Declares the schema for the OpenAPI Schema type (and its dependent types) and
-- returns a reference that can be used to refer to it from other schemas.
--
-- This is a transcription of the schemas defined here:
-- https://raw.githubusercontent.com/OAI/OpenAPI-Specification/80c781e479f85ac67001ceb3e7e410e25d2a561b/schemas/v3.0/schema.json#/definitions/Schema
--
-- Unfortunately using external references to the above schema tends to make many
-- OpenAPI type generators choke, so importing the relevant schemas into our spec
-- is a pragmatic workaround.
declareOpenApiSchema :: Declare (Definitions Schema) (Referenced Schema)
declareOpenApiSchema = do
  declare $
    InsOrdHashMap.fromList
      [ openApiSchema,
        openApiReference,
        openApiDiscriminator,
        openApiExternalDocumentation,
        openApiXml
      ]
  pure . Ref $ Reference "OpenApiSchema"
  where
    openApiSchema :: (Text, Schema)
    openApiSchema =
      ( "OpenApiSchema",
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaProperties =
              InsOrdHashMap.fromList
                [ ("title", Inline mempty {_schemaType = Just OpenApiString}),
                  ("multipleOf", Inline mempty {_schemaType = Just OpenApiNumber, _schemaMinimum = Just 0, _schemaExclusiveMinimum = Just True}),
                  ("maximum", Inline mempty {_schemaType = Just OpenApiNumber}),
                  ("exclusiveMaximum", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("minimum", Inline mempty {_schemaType = Just OpenApiNumber}),
                  ("exclusiveMinimum", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("maxLength", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0}),
                  ("minLength", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0, _schemaDefault = Just $ Number 0}),
                  ("pattern", Inline mempty {_schemaType = Just OpenApiString, _schemaFormat = Just "regex"}),
                  ("maxItems", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0}),
                  ("minItems", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0, _schemaDefault = Just $ Number 0}),
                  ("uniqueItems", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("maxProperties", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0}),
                  ("minProperties", Inline mempty {_schemaType = Just OpenApiInteger, _schemaMinimum = Just 0, _schemaDefault = Just $ Number 0}),
                  ( "required",
                    Inline
                      mempty
                        { _schemaType = Just OpenApiArray,
                          _schemaItems = Just . OpenApiItemsObject $ Inline mempty {_schemaType = Just OpenApiString},
                          _schemaMinItems = Just 1,
                          _schemaUniqueItems = Just True
                        }
                  ),
                  ( "enum",
                    Inline
                      mempty
                        { _schemaType = Just OpenApiArray,
                          _schemaItems = Just . OpenApiItemsObject $ Inline mempty,
                          _schemaMinItems = Just 1,
                          _schemaUniqueItems = Just False
                        }
                  ),
                  ("type", Inline mempty {_schemaType = Just OpenApiString, _schemaEnum = Just ["array", "boolean", "integer", "number", "object", "string"]}),
                  ("not", Inline mempty {_schemaOneOf = Just schemaOrReference}),
                  ("allOf", Inline mempty {_schemaType = Just OpenApiArray, _schemaItems = Just . OpenApiItemsObject $ Inline mempty {_schemaOneOf = Just schemaOrReference}}),
                  ("oneOf", Inline mempty {_schemaType = Just OpenApiArray, _schemaItems = Just . OpenApiItemsObject $ Inline mempty {_schemaOneOf = Just schemaOrReference}}),
                  ("anyOf", Inline mempty {_schemaType = Just OpenApiArray, _schemaItems = Just . OpenApiItemsObject $ Inline mempty {_schemaOneOf = Just schemaOrReference}}),
                  ("items", Inline mempty {_schemaOneOf = Just schemaOrReference}),
                  ("properties", Inline mempty {_schemaType = Just OpenApiObject, _schemaAdditionalProperties = Just . AdditionalPropertiesSchema $ Inline mempty {_schemaOneOf = Just schemaOrReference}}),
                  ( "additionalProperties",
                    Inline
                      mempty
                        { _schemaAdditionalProperties = Just . AdditionalPropertiesSchema $ Inline mempty {_schemaOneOf = Just $ schemaOrReference <> [Inline mempty {_schemaType = Just OpenApiBoolean}]},
                          _schemaDefault = Just $ Bool True
                        }
                  ),
                  ("description", Inline mempty {_schemaType = Just OpenApiString}),
                  ("format", Inline mempty {_schemaType = Just OpenApiString}),
                  ("default", Inline mempty),
                  ("nullable", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("discriminator", Ref . Reference $ fst openApiDiscriminator),
                  ("readOnly", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("writeOnly", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("example", Inline mempty),
                  ("externalDocs", Ref . Reference $ fst openApiExternalDocumentation),
                  ("deprecated", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("xml", Ref . Reference $ fst openApiXml)
                ],
            -- Note: Technically OpenAPI schemas should be able to define extension properties but since OpenAPI itself doesn't
            -- support defining patternProperties, I can't define them here. 😢
            -- "patternProperties": { "^x-": {} }
            -- _schemaPatternProperties =
            _schemaAdditionalProperties = Just $ AdditionalPropertiesAllowed False
          }
      )

    openApiReference :: (Text, Schema)
    openApiReference =
      ( "OpenApiReference",
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaRequired = ["$ref"],
            -- Note: This is technically defined using "patternProperties" with the property name regex ^\$ref$
            -- but OpenAPI doesn't support patternProperties ironically, so this is close enough
            _schemaProperties = InsOrdHashMap.fromList [("$ref", Inline mempty {_schemaType = Just OpenApiString, _schemaFormat = Just "uri-reference"})]
          }
      )

    schemaOrReference :: [Referenced Schema]
    schemaOrReference = [Ref . Reference $ fst openApiSchema, Ref . Reference $ fst openApiReference]

    openApiDiscriminator :: (Text, Schema)
    openApiDiscriminator =
      ( "OpenApiDiscriminator",
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaRequired = ["propertyName"],
            _schemaProperties =
              InsOrdHashMap.fromList
                [ ("propertyName", Inline mempty {_schemaType = Just OpenApiString}),
                  ("mapping", Inline mempty {_schemaType = Just OpenApiObject, _schemaAdditionalProperties = Just . AdditionalPropertiesSchema $ Inline mempty {_schemaType = Just OpenApiString}})
                ]
          }
      )

    openApiExternalDocumentation :: (Text, Schema)
    openApiExternalDocumentation =
      ( "OpenApiExternalDocumentation",
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaRequired = ["url"],
            _schemaProperties =
              InsOrdHashMap.fromList
                [ ("description", Inline mempty {_schemaType = Just OpenApiString}),
                  ("url", Inline mempty {_schemaType = Just OpenApiString, _schemaFormat = Just "uri-reference"})
                ],
            -- Note: Technically external docs should be able to define extension properties but since OpenAPI itself doesn't
            -- support defining patternProperties, I can't define them here. 😢
            -- "patternProperties": { "^x-": {} }
            -- _schemaPatternProperties =
            _schemaAdditionalProperties = Just $ AdditionalPropertiesAllowed False
          }
      )

    openApiXml :: (Text, Schema)
    openApiXml =
      ( "OpenApiXml",
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaProperties =
              InsOrdHashMap.fromList
                [ ("name", Inline mempty {_schemaType = Just OpenApiString}),
                  ("namespace", Inline mempty {_schemaType = Just OpenApiString, _schemaFormat = Just "uri"}),
                  ("prefix", Inline mempty {_schemaType = Just OpenApiString}),
                  ("attribute", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False}),
                  ("wrapped", Inline mempty {_schemaType = Just OpenApiBoolean, _schemaDefault = Just $ Bool False})
                ],
            -- Note: Technically XML should be able to define extension properties but since OpenAPI itself doesn't
            -- support defining patternProperties, I can't define them here. 😢
            -- "patternProperties": { "^x-": {} }
            -- _schemaPatternProperties =
            _schemaAdditionalProperties = Just $ AdditionalPropertiesAllowed False
          }
      )

-- | Rewrites the config schema internal refs to the form that openapi3 expects when it deserialized them
--
-- This works around a limitation of the openapi3 library where it expects that all refs will be pointing
-- to the place in the overall document where those particular things are normally stored on specifically
-- the 'OpenApi' type and nothing else.
-- This means that it cannot understand refs like #/otherSchemas/Thing, and must see #/components/schemas/Thing
-- to correctly deserialise
rewriteConfigSchemaRefsToOpenApiRefs :: Value -> Value
rewriteConfigSchemaRefsToOpenApiRefs = rewriteSchemaRefs configSchemaToOpenApiSchemaRef

configSchemaToOpenApiSchemaRef :: Text -> Text
configSchemaToOpenApiSchemaRef = \case
  (Text.stripPrefix "#/other_schemas/" -> Just suffix) -> "#/components/schemas/" <> suffix
  other -> other

-- | Rewrites the refs that openapi3 serializes to their proper pathing given their actual location
-- in the 'ConfigSchemaResponse' type.
--
-- This works around a limitation of the openapi3 library where it expects that all refs will be pointing
-- to the place in the overall document where those particular things are normally stored on specifically
-- the 'OpenApi' type and nothing else.
rewriteOpenApiRefsToConfigSchemaRefs :: Value -> Value
rewriteOpenApiRefsToConfigSchemaRefs = rewriteSchemaRefs openApiSchemaToConfigSchemaRef

openApiSchemaToConfigSchemaRef :: Text -> Text
openApiSchemaToConfigSchemaRef = \case
  (Text.stripPrefix "#/components/schemas/" -> Just suffix) -> "#/other_schemas/" <> suffix
  other -> other

rewriteSchemaRefs :: (Text -> Text) -> Value -> Value
rewriteSchemaRefs rewriteRefText schemaObj =
  schemaObj
    & key "allOf" . values %~ rewriteRef
    & key "oneOf" . values %~ rewriteRef
    & key "not" %~ rewriteRef
    & key "anyOf" . values %~ rewriteRef
    & key "properties" . members %~ rewriteRef
    & key "additionalProperties" %~ rewriteRef
    & key "items" %~ rewriteRef -- if its an Object
    & key "items" . values %~ rewriteRef -- if its an Array
  where
    rewriteRef :: Value -> Value
    rewriteRef refOrInlineSchema =
      -- If its $ref rewrite it, otherwise it's an inline schema, so recurse
      fromMaybe (rewriteSchemaRefs rewriteRefText refOrInlineSchema) $ tryRewriteRef refOrInlineSchema

    tryRewriteRef :: Value -> Maybe Value
    tryRewriteRef refOrInlineSchema = do
      refText <- refOrInlineSchema ^? key "$ref" . _String
      pure $ refOrInlineSchema & key "$ref" . _String .~ rewriteRefText refText

validateConfigAgainstConfigSchema :: ConfigSchemaResponse -> Config -> [ValidationError]
validateConfigAgainstConfigSchema ConfigSchemaResponse {..} (Config config) =
  OpenApi.validateJSON _csrOtherSchemas _csrConfigSchema (Object config)
