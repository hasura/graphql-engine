{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.DataConnector.API.V0.ConfigSchema
  ( Config (..),
    ConfigSchemaResponse (..),
    validateConfigAgainstConfigSchema,
    fixExternalSchemaRefsInComponentSchemas,
    fixExternalSchemaRefsInSchema,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens ((%~), (&), (.~), (^?))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), encode, object, withObject, (.:), (.=), (<?>))
import Data.Aeson.Lens (AsPrimitive (..), key, members, values)
import Data.Aeson.Types (JSONPathElement (..))
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.OpenApi (AdditionalProperties (..), Definitions, NamedSchema (..), OpenApiType (..), Reference (..), Referenced (..), Schema (..), ToParamSchema (..), ToSchema (..), ValidationError)
import Data.OpenApi qualified as OpenApi
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant.API (ToHttpApiData (..))
import Prelude

newtype Config = Config {unConfig :: Object}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, NFData, ToJSON, FromJSON)

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
  deriving (Show, Eq)

instance FromJSON ConfigSchemaResponse where
  parseJSON = withObject "ConfigSchemaResponse" $ \obj -> do
    configSchemaValue <- obj .: "configSchema"
    (otherSchemaValues :: Object) <- obj .: "otherSchemas"
    _csrConfigSchema <- parseJSON (rewriteConfigSchemaRefsToOpenApiRefs configSchemaValue) <?> Key "configSchema"
    _csrOtherSchemas <- (<?> Key "otherSchemas") . parseJSON . Object $ rewriteConfigSchemaRefsToOpenApiRefs <$> otherSchemaValues
    pure ConfigSchemaResponse {..}

instance ToJSON ConfigSchemaResponse where
  toJSON ConfigSchemaResponse {..} =
    let configSchemaValue = rewriteOpenApiRefsToConfigSchemaRefs $ toJSON _csrConfigSchema
        otherSchemasValue = rewriteOpenApiRefsToConfigSchemaRefs . toJSON <$> _csrOtherSchemas
     in object
          [ "configSchema" .= configSchemaValue,
            "otherSchemas" .= otherSchemasValue
          ]

instance ToSchema ConfigSchemaResponse where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "ConfigSchemaResponse") schema
    where
      schema :: Schema
      schema =
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaNullable = Just False,
            _schemaRequired = ["configSchema", "otherSchemas"],
            _schemaProperties =
              InsOrdHashMap.fromList
                [ ("configSchema", openApiSchemaSchema),
                  ("otherSchemas", Inline otherSchemasSchema)
                ]
          }

      otherSchemasSchema :: Schema
      otherSchemasSchema =
        mempty
          { _schemaType = Just OpenApiObject,
            _schemaNullable = Just False,
            _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema openApiSchemaSchema
          }

      openApiSchemaSchema :: Referenced Schema
      openApiSchemaSchema =
        Ref (Reference "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/80c781e479f85ac67001ceb3e7e410e25d2a561b/schemas/v3.0/schema.json#/definitions/Schema")

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
  (Text.stripPrefix "#/otherSchemas/" -> Just suffix) -> "#/components/schemas/" <> suffix
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
  (Text.stripPrefix "#/components/schemas/" -> Just suffix) -> "#/otherSchemas/" <> suffix
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

-- | Fixes any refs in schemas that are external refs to an http-based URL.
-- Note that this is limited to schemas in the components/schemas section.
-- This is used to specifically address the external refs defined by the
-- OpenAPI schema spec of the 'ConfigSchemaResponse' type.
--
-- This works around a limitation in the openapi3 library where it does not
-- understand the concept of external refs and will always assume any defined
-- ref refers to a schema inside the top level OpenApi document itself.
-- Practically, this means that #/components/schemas/ gets mashed onto the
-- front of any external ref :(
fixExternalSchemaRefsInComponentSchemas :: Value -> Value
fixExternalSchemaRefsInComponentSchemas openApiObj =
  openApiObj
    & key "components" . key "schemas" . members %~ fixExternalSchemaRefsInSchema

fixExternalSchemaRefsInSchema :: Value -> Value
fixExternalSchemaRefsInSchema = rewriteSchemaRefs fixExternalHttpSchemaRef

fixExternalHttpSchemaRef :: Text -> Text
fixExternalHttpSchemaRef = \case
  (Text.stripPrefix "#/components/schemas/http" -> Just suffix) -> "http" <> suffix
  other -> other
