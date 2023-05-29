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
    twtRequestTransformer,
    twtResponseTransformer,
    WebHookUrl (..),

    -- * Dump Internal State
    DumpInternalState (..),
  )
where

--------------------------------------------------------------------------------

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KeyMap
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorName)
import Hasura.Prelude
import Hasura.RQL.DDL.Warnings (AllowWarnings (..))
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata (Metadata, MetadataNoSources)
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RemoteSchema.Metadata (RemoteSchemaName)
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
  deriving (Show, Generic, Eq)

instance J.ToJSON ClearMetadata where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

-- | 'ExportMetadata' is used to export the current metadata from the
-- server as a JSON file.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-export-metadata
data ExportMetadata = ExportMetadata deriving (Show, Eq)

instance ToJSON ExportMetadata where
  toJSON ExportMetadata = J.object []

instance FromJSON ExportMetadata where
  parseJSON _ = pure ExportMetadata

data ReloadSpec a
  = RSReloadAll
  | RSReloadList (HashSet a)
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (ReloadSpec a) where
  toJSON = \case
    RSReloadAll -> J.Bool True
    RSReloadList l -> J.toJSON l

instance (FromJSON a, Hashable a) => FromJSON (ReloadSpec a) where
  parseJSON (J.Bool b) = pure $ if b then RSReloadAll else RSReloadList mempty
  parseJSON v = RSReloadList <$> J.parseJSON v

type ReloadRemoteSchemas = ReloadSpec RemoteSchemaName

type ReloadSources = ReloadSpec Common.SourceName

type ReloadDataConnectors = ReloadSpec DataConnectorName

reloadAllRemoteSchemas :: ReloadRemoteSchemas
reloadAllRemoteSchemas = RSReloadAll

reloadAllSources :: ReloadSources
reloadAllSources = RSReloadAll

reloadAllDataConnectors :: ReloadDataConnectors
reloadAllDataConnectors = RSReloadAll

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
    _rmRecreateEventTriggers :: ReloadSources,
    _rmReloadDataConnectors :: ReloadDataConnectors
  }
  deriving (Show, Generic, Eq)

instance J.ToJSON ReloadMetadata where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance FromJSON ReloadMetadata where
  parseJSON = J.withObject "ReloadMetadata" $ \o ->
    ReloadMetadata
      <$> o
      .:? "reload_remote_schemas"
      .!= reloadAllRemoteSchemas
      <*> o
      .:? "reload_sources"
      .!= reloadAllSources
      <*> o
      .:? "recreate_event_triggers"
      .!= RSReloadList mempty
      <*> o
      .:? "reload_data_connectors"
      .!= reloadAllDataConnectors

-- | Undocumented Metadata API action which serializes the entire
-- 'SchemaCache'.
data DumpInternalState
  = DumpInternalState
  deriving (Show, Generic, Eq)

instance J.ToJSON DumpInternalState where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

-- | 'GetInconsistentMetadata' can be used to fetch all inconsistent metadata objects.
--
-- https://hasura.io/docs/latest/api-reference/schema-metadata-api/manage-metadata/#schema-metadata-get-inconsistent-metadata
data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Generic, Eq)

instance J.ToJSON GetInconsistentMetadata where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

-- | 'DropInconsistentMetadata' can be used to purge all inconsistent
-- objects from the metadata.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-drop-inconsistent-metadata
data DropInconsistentMetadata
  = DropInconsistentMetadata
  deriving (Show, Generic, Eq)

instance J.ToJSON DropInconsistentMetadata where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

data AllowInconsistentMetadata
  = AllowInconsistentMetadata
  | NoAllowInconsistentMetadata
  deriving (Show, Eq)

instance FromJSON AllowInconsistentMetadata where
  parseJSON =
    J.withBool "AllowInconsistentMetadata"
      $ pure
      . bool NoAllowInconsistentMetadata AllowInconsistentMetadata

instance ToJSON AllowInconsistentMetadata where
  toJSON = J.toJSON . toBool
    where
      toBool AllowInconsistentMetadata = True
      toBool NoAllowInconsistentMetadata = False

-- | Replace metadata either with or without metadata sources.
data ReplaceMetadataV1
  = RMWithSources Metadata
  | RMWithoutSources MetadataNoSources
  deriving (Eq)

instance FromJSON ReplaceMetadataV1 where
  parseJSON = J.withObject "ReplaceMetadataV1" $ \o -> do
    version <- o .:? "version" .!= Metadata.MVVersion1
    case version of
      Metadata.MVVersion3 -> RMWithSources <$> J.parseJSON (J.Object o)
      _ -> RMWithoutSources <$> J.parseJSON (J.Object o)

instance ToJSON ReplaceMetadataV1 where
  toJSON = \case
    RMWithSources v -> J.toJSON v
    RMWithoutSources v -> J.toJSON v

-- | Replace metadata while allowing for inconsitency in the metadata object.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#metadata-replace-metadata-syntax
data ReplaceMetadataV2 = ReplaceMetadataV2
  { _rmv2AllowInconsistentMetadata :: AllowInconsistentMetadata,
    _rmv2AllowWarningss :: AllowWarnings,
    _rmv2Metadata :: ReplaceMetadataV1
  }
  deriving (Eq)

instance FromJSON ReplaceMetadataV2 where
  parseJSON = J.withObject "ReplaceMetadataV2" $ \o ->
    ReplaceMetadataV2
      <$> o
      .:? "allow_inconsistent_metadata"
      .!= NoAllowInconsistentMetadata
      <*> o
      .:? "allow_warnings"
      .!= AllowWarnings
      <*> o
      .: "metadata"

instance ToJSON ReplaceMetadataV2 where
  toJSON ReplaceMetadataV2 {..} =
    J.object
      [ "allow_inconsistent_metadata" .= _rmv2AllowInconsistentMetadata,
        "allow_warnings" .= _rmv2AllowWarningss,
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
  parseJSON = J.withObject "ReplaceMetadata" $ \o -> do
    if KeyMap.member "metadata" o
      then RMReplaceMetadataV2 <$> J.parseJSON (J.Object o)
      else RMReplaceMetadataV1 <$> J.parseJSON (J.Object o)

instance ToJSON ReplaceMetadata where
  toJSON = \case
    RMReplaceMetadataV1 v1 -> J.toJSON v1
    RMReplaceMetadataV2 v2 -> J.toJSON v2

data WebHookUrl = EnvVar String | URL Text
  deriving (Eq)

instance FromJSON WebHookUrl where
  parseJSON (J.Object o) = do
    var <- o .: "from_env"
    pure $ EnvVar var
  parseJSON (J.String str) = pure $ URL str
  parseJSON _ = empty

instance ToJSON WebHookUrl where
  toJSON (EnvVar var) = J.object ["from_env" .= var]
  toJSON (URL url) = J.toJSON url

-- | 'TestWebhookTransform' can be used to test out request
-- transformations using mock data.
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#test-webhook-transform
data TestWebhookTransform = TestWebhookTransform
  { _twtEnv :: Env.Environment,
    _twtHeaders :: [HTTP.Header],
    _twtWebhookUrl :: WebHookUrl,
    _twtPayload :: J.Value,
    _twtTransformer :: RequestTransform,
    _twtResponseTransformer :: Maybe MetadataResponseTransform,
    _twtSessionVariables :: Maybe SessionVariables
  }
  deriving (Eq)

twtRequestTransformer :: Lens' TestWebhookTransform RequestTransform
twtRequestTransformer = Lens.lens _twtTransformer \twt a -> twt {_twtTransformer = a}

twtResponseTransformer :: Lens' TestWebhookTransform (Maybe MetadataResponseTransform)
twtResponseTransformer = Lens.lens _twtResponseTransformer \twt a -> twt {_twtResponseTransformer = a}

instance FromJSON TestWebhookTransform where
  parseJSON = J.withObject "TestWebhookTransform" $ \o -> do
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
    J.object
      [ "env" .= env,
        "request_headers" .= fmap (first CI.original) headers,
        "webhook_url" .= url,
        "body" .= payload,
        "request_transform" .= mt,
        "response_transform" .= mrt,
        "session_variables" .= sv
      ]
