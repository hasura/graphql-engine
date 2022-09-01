{-# LANGUAGE TemplateHaskell #-}

-- | Types related to metadata management API
module Hasura.RQL.DDL.Metadata.Types
  ( -- * Export Metadata
    ExportMetadata (..),

    -- * Replace Metadata
    ReplaceMetadata (..),
    ReplaceMetadataV1 (..),
    ReplaceMetadataV2 (..),
    AllowInconsistentMetadata (..),

    -- * Reload Metadata
    ReloadMetadata (..),
    ReloadSpec (..),

    -- * Clear Metadata
    ClearMetadata (..),

    -- * Get Inconsistent Metadata
    GetInconsistentMetadata (..),

    -- * Drop Inconsistent Metadata
    DropInconsistentMetadata (..),

    -- * Test Webhook Transform
    TestWebhookTransform (..),
    twtTransformer,
    WebHookUrl (..),

    -- * Dump Internal State
    DumpInternalState (..),
  )
where

--------------------------------------------------------------------------------

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH qualified as Aeson.TH
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata (Metadata, MetadataNoSources)
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.RemoteSchema (RemoteSchemaName)
import Hasura.Session (SessionVariables)
import Network.HTTP.Client.Transformable qualified as HTTP

--------------------------------------------------------------------------------

-- | 'ClearMetadata' can be used to reset the state of Hasura -- clean
-- the current state by forgetting the tables tracked, relationships,
-- permissions, event triggers etc.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-clear-metadata
data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq)

$(Aeson.TH.deriveToJSON Aeson.TH.defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

-- | 'ExportMetadata' is used to export the current metadata from the
-- server as a JSON file.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-export-metadata
data ExportMetadata = ExportMetadata deriving (Show, Eq)

instance ToJSON ExportMetadata where
  toJSON ExportMetadata = Aeson.object []

instance FromJSON ExportMetadata where
  parseJSON _ = pure ExportMetadata

data ReloadSpec a
  = RSReloadAll
  | RSReloadList (HashSet a)
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (ReloadSpec a) where
  toJSON = \case
    RSReloadAll -> Aeson.Bool True
    RSReloadList l -> Aeson.toJSON l

instance (FromJSON a, Eq a, Hashable a) => FromJSON (ReloadSpec a) where
  parseJSON (Aeson.Bool b) = pure $ if b then RSReloadAll else RSReloadList mempty
  parseJSON v = RSReloadList <$> Aeson.parseJSON v

type ReloadRemoteSchemas = ReloadSpec RemoteSchemaName

type ReloadSources = ReloadSpec Common.SourceName

reloadAllRemoteSchemas :: ReloadRemoteSchemas
reloadAllRemoteSchemas = RSReloadAll

reloadAllSources :: ReloadSources
reloadAllSources = RSReloadAll

-- | 'ReloadMetadata' should be used when there is a change in
-- underlying Postgres database that Hasura should be aware
-- of. Example: a new column is added to a table using psql and this
-- column should now be added to the GraphQL schema.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-reload-metadata
data ReloadMetadata = ReloadMetadata
  { _rmReloadRemoteSchemas :: ReloadRemoteSchemas,
    _rmReloadSources :: ReloadSources,
    -- | Provides a way for the user to allow to explicitly recreate event triggers
    --   for some or all the sources. This is useful when a user may have fiddled with
    --   the SQL trigger in the source and they'd simply want the event trigger to be
    --   recreated without deleting and creating the event trigger. By default, no
    --   source's event triggers will be recreated.
    _rmRecreateEventTriggers :: ReloadSources
  }
  deriving (Show, Eq)

$(Aeson.TH.deriveToJSON hasuraJSON ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = Aeson.withObject "ReloadMetadata" $ \o ->
    ReloadMetadata
      <$> o .:? "reload_remote_schemas" .!= reloadAllRemoteSchemas
      <*> o .:? "reload_sources" .!= reloadAllSources
      <*> o .:? "recreate_event_triggers" .!= RSReloadList mempty

-- | Undocumented Metadata API action which serializes the entire
-- 'SchemaCache'.
data DumpInternalState
  = DumpInternalState
  deriving (Show, Eq)

$(Aeson.TH.deriveToJSON Aeson.TH.defaultOptions ''DumpInternalState)

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

-- | 'GetInconsistentMetadata' can be used to fetch all inconsistent metadata objects.
--
-- https://hasura.io/docs/latest/api-reference/schema-metadata-api/manage-metadata/#schema-metadata-get-inconsistent-metadata
data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Eq)

$(Aeson.TH.deriveToJSON Aeson.TH.defaultOptions ''GetInconsistentMetadata)

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

-- | 'DropInconsistentMetadata' can be used to purge all inconsistent
-- objects from the metadata.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-drop-inconsistent-metadata
data DropInconsistentMetadata
  = DropInconsistentMetadata
  deriving (Show, Eq)

$(Aeson.TH.deriveToJSON Aeson.TH.defaultOptions ''DropInconsistentMetadata)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

data AllowInconsistentMetadata
  = AllowInconsistentMetadata
  | NoAllowInconsistentMetadata
  deriving (Show, Eq)

instance FromJSON AllowInconsistentMetadata where
  parseJSON =
    Aeson.withBool "AllowInconsistentMetadata" $
      pure . bool NoAllowInconsistentMetadata AllowInconsistentMetadata

instance ToJSON AllowInconsistentMetadata where
  toJSON = Aeson.toJSON . toBool
    where
      toBool AllowInconsistentMetadata = True
      toBool NoAllowInconsistentMetadata = False

-- | Replace metadata either with or without metadata sources.
data ReplaceMetadataV1
  = RMWithSources Metadata
  | RMWithoutSources MetadataNoSources
  deriving (Eq)

instance FromJSON ReplaceMetadataV1 where
  parseJSON = Aeson.withObject "ReplaceMetadataV1" $ \o -> do
    version <- o .:? "version" .!= Metadata.MVVersion1
    case version of
      Metadata.MVVersion3 -> RMWithSources <$> Aeson.parseJSON (Aeson.Object o)
      _ -> RMWithoutSources <$> Aeson.parseJSON (Aeson.Object o)

instance ToJSON ReplaceMetadataV1 where
  toJSON = \case
    RMWithSources v -> Aeson.toJSON v
    RMWithoutSources v -> Aeson.toJSON v

-- | Replace metadata while allowing for inconsitency in the metadata object.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-replace-metadata-syntax
data ReplaceMetadataV2 = ReplaceMetadataV2
  { _rmv2AllowInconsistentMetadata :: AllowInconsistentMetadata,
    _rmv2Metadata :: ReplaceMetadataV1
  }
  deriving (Eq)

instance FromJSON ReplaceMetadataV2 where
  parseJSON = Aeson.withObject "ReplaceMetadataV2" $ \o ->
    ReplaceMetadataV2
      <$> o .:? "allow_inconsistent_metadata" .!= NoAllowInconsistentMetadata
      <*> o .: "metadata"

instance ToJSON ReplaceMetadataV2 where
  toJSON ReplaceMetadataV2 {..} =
    Aeson.object
      [ "allow_inconsistent_metadata" .= _rmv2AllowInconsistentMetadata,
        "metadata" .= _rmv2Metadata
      ]

-- | 'ReplaceMetadata' is used to replace/import metadata into
-- Hasura. Existing metadata will be replaced with the new one.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-replace-metadata
-- TODO: If additional API versions are supported in future it would
-- be ideal to include a version field Rather than differentiating on
-- the "metadata" field.
data ReplaceMetadata
  = RMReplaceMetadataV1 ReplaceMetadataV1
  | RMReplaceMetadataV2 ReplaceMetadataV2
  deriving (Eq)

instance FromJSON ReplaceMetadata where
  parseJSON = Aeson.withObject "ReplaceMetadata" $ \o -> do
    if KeyMap.member "metadata" o
      then RMReplaceMetadataV2 <$> Aeson.parseJSON (Aeson.Object o)
      else RMReplaceMetadataV1 <$> Aeson.parseJSON (Aeson.Object o)

instance ToJSON ReplaceMetadata where
  toJSON = \case
    RMReplaceMetadataV1 v1 -> Aeson.toJSON v1
    RMReplaceMetadataV2 v2 -> Aeson.toJSON v2

data WebHookUrl = EnvVar String | URL Text
  deriving (Eq)

instance FromJSON WebHookUrl where
  parseJSON (Aeson.Object o) = do
    var <- o .: "from_env"
    pure $ EnvVar var
  parseJSON (Aeson.String str) = pure $ URL str
  parseJSON _ = empty

instance ToJSON WebHookUrl where
  toJSON (EnvVar var) = Aeson.object ["from_env" .= var]
  toJSON (URL url) = Aeson.toJSON url

-- | 'TestWebhookTransform' can be used to test out request
-- transformations using mock data.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#test-webhook-transform
data TestWebhookTransform = TestWebhookTransform
  { _twtEnv :: Env.Environment,
    _twtHeaders :: [HTTP.Header],
    _twtWebhookUrl :: WebHookUrl,
    _twtPayload :: Aeson.Value,
    _twtTransformer :: RequestTransform,
    _twtResponseTransformer :: Maybe MetadataResponseTransform,
    _twtSessionVariables :: Maybe SessionVariables
  }
  deriving (Eq)

twtTransformer :: Lens' TestWebhookTransform RequestTransform
twtTransformer = Lens.lens _twtTransformer \twt a -> twt {_twtTransformer = a}

instance FromJSON TestWebhookTransform where
  parseJSON = Aeson.withObject "TestWebhookTransform" $ \o -> do
    env <- fmap (fromMaybe mempty) $ o .:? "env"
    headers <- fmap (fmap (first (CI.mk))) $ o .:? "request_headers" .!= []
    url <- o .: "webhook_url"
    payload <- o .: "body"
    reqTransform <- o .: "request_transform"
    respTransform <- o .:? "response_transform"
    sessionVars <- o .:? "session_variables"
    pure $ TestWebhookTransform env headers url payload reqTransform respTransform sessionVars

instance ToJSON TestWebhookTransform where
  toJSON (TestWebhookTransform env headers url payload mt mrt sv) =
    Aeson.object
      [ "env" .= env,
        "request_headers" .= fmap (first CI.original) headers,
        "webhook_url" .= url,
        "body" .= payload,
        "request_transform" .= mt,
        "response_transform" .= mrt,
        "session_variables" .= sv
      ]
