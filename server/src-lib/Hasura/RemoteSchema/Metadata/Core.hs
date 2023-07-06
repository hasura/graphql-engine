{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.RemoteSchema.Metadata.Core
  ( RemoteSchemaDef (..),
    RemoteSchemaName (..),
    UrlFromEnv,
    getUrlFromEnv,
    RemoteSchemaMetadataG (..),
    rsmComment,
    rsmDefinition,
    rsmName,
    rsmPermissions,
    rsmRemoteRelationships,
  )
where

import Autodocodec (object, optionalField', optionalFieldWithDefault', optionalFieldWithDefaultWith', requiredField', (.=))
import Autodocodec.Class (HasCodec (codec))
import Autodocodec.Extended (typeableName)
import Control.Lens (makeLenses)
import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd.Autodocodec (insertionOrderedElemsCodec)
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Headers (HeaderConf (..))
import Hasura.RemoteSchema.Metadata.Base
import Hasura.RemoteSchema.Metadata.Customization
import Hasura.RemoteSchema.Metadata.Permission
import Hasura.RemoteSchema.Metadata.RemoteRelationship
import Network.URI.Extended qualified as N

type UrlFromEnv = Text

-- | Unvalidated remote schema config, from the user's API request
data RemoteSchemaDef = RemoteSchemaDef
  { _rsdUrl :: Maybe InputWebhook,
    _rsdUrlFromEnv :: Maybe UrlFromEnv,
    _rsdHeaders :: Maybe [HeaderConf],
    _rsdForwardClientHeaders :: Bool,
    _rsdTimeoutSeconds :: Maybe Int,
    _rsdCustomization :: Maybe RemoteSchemaCustomization
    -- NOTE: In the future we might extend this API to support a small DSL of
    -- name transformations; this might live at a different layer, and be part of
    -- the schema customization story.
    --
    -- See: https://github.com/hasura/graphql-engine-mono/issues/144
    -- TODO we probably want to move this into a sub-field "transformations"?
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaDef

instance HasCodec RemoteSchemaDef where
  codec =
    object "RemoteSchemaDef"
      $ RemoteSchemaDef
      <$> optionalField' "url"
      .= _rsdUrl
        <*> optionalField' "url_from_env"
      .= _rsdUrlFromEnv
        <*> optionalField' "headers"
      .= _rsdHeaders
        <*> optionalFieldWithDefault' "forward_client_headers" False
      .= _rsdForwardClientHeaders
        <*> optionalField' "timeout_seconds"
      .= _rsdTimeoutSeconds
        <*> optionalField' "customization"
      .= _rsdCustomization

instance J.ToJSON RemoteSchemaDef where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "Object" $ \o ->
    RemoteSchemaDef
      <$> o
      J..:? "url"
      <*> o
      J..:? "url_from_env"
      <*> o
      J..:? "headers"
      <*> o
      J..:? "forward_client_headers"
      J..!= False
      <*> o
      J..:? "timeout_seconds"
      <*> o
      J..:? "customization"

getUrlFromEnv :: (MonadError QErr m) => Env.Environment -> Text -> m (EnvRecord N.URI)
getUrlFromEnv env urlFromEnv = do
  let mEnv = Env.lookupEnv env $ T.unpack urlFromEnv
  uri <- onNothing mEnv (throw400 InvalidParams $ envNotFoundMsg urlFromEnv)
  case (N.parseURI uri) of
    Just uri' -> pure $ EnvRecord urlFromEnv uri'
    Nothing -> throw400 InvalidParams $ invalidUri urlFromEnv
  where
    invalidUri x = "not a valid URI in environment variable: " <> x
    envNotFoundMsg e = "environment variable '" <> e <> "' not set"

data RemoteSchemaMetadataG r = RemoteSchemaMetadata
  { _rsmName :: RemoteSchemaName,
    _rsmDefinition :: RemoteSchemaDef,
    _rsmComment :: Maybe Text,
    _rsmPermissions :: [RemoteSchemaPermissionMetadata],
    _rsmRemoteRelationships :: (SchemaRemoteRelationships r)
  }
  deriving (Show, Eq, Generic)

instance (HasCodec (RemoteRelationshipG r), Typeable r) => HasCodec (RemoteSchemaMetadataG r) where
  codec =
    object ("RemoteSchemaMetadata_" <> typeableName @r)
      $ RemoteSchemaMetadata
      <$> requiredField' "name"
      .= _rsmName
        <*> requiredField' "definition"
      .= _rsmDefinition
        <*> optionalField' "comment"
      .= _rsmComment
        <*> optionalFieldWithDefault' "permissions" mempty
      .= _rsmPermissions
        <*> optionalFieldWithDefaultWith'
          "remote_relationships"
          (insertionOrderedElemsCodec _rstrsName)
          mempty
      .= _rsmRemoteRelationships

instance (J.FromJSON (RemoteRelationshipG r)) => J.FromJSON (RemoteSchemaMetadataG r) where
  parseJSON = J.withObject "RemoteSchemaMetadata" \obj ->
    RemoteSchemaMetadata
      <$> obj
      J..: "name"
      <*> obj
      J..: "definition"
      <*> obj
      J..:? "comment"
      <*> obj
      J..:? "permissions"
      J..!= mempty
      <*> (oMapFromL _rstrsName <$> obj J..:? "remote_relationships" J..!= [])

instance (J.ToJSON (RemoteRelationshipG r)) => J.ToJSON (RemoteSchemaMetadataG r) where
  toJSON RemoteSchemaMetadata {..} =
    J.object
      [ "name" J..= _rsmName,
        "definition" J..= _rsmDefinition,
        "comment" J..= _rsmComment,
        "permissions" J..= _rsmPermissions,
        "remote_relationships" J..= InsOrdHashMap.elems _rsmRemoteRelationships
      ]

$(makeLenses ''RemoteSchemaMetadataG)
