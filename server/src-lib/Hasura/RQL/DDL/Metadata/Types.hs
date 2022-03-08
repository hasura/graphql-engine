-- | Types related to metadata management API
module Hasura.RQL.DDL.Metadata.Types
  ( currentMetadataVersion,
    MetadataVersion (..),
    ExportMetadata (..),
    ClearMetadata (..),
    ReloadSpec (..),
    ReloadMetadata (..),
    DumpInternalState (..),
    GetInconsistentMetadata (..),
    DropInconsistentMetadata (..),
    ReplaceMetadata (..),
    ReplaceMetadataV1 (..),
    ReplaceMetadataV2 (..),
    AllowInconsistentMetadata (..),
    WebHookUrl (..),
    TestWebhookTransform (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as H
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types
import Hasura.Session (SessionVariables)
import Network.HTTP.Client.Transformable qualified as HTTP

data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

data ExportMetadata = ExportMetadata deriving (Show, Eq)

instance ToJSON ExportMetadata where
  toJSON ExportMetadata = object []

instance FromJSON ExportMetadata where
  parseJSON _ = pure ExportMetadata

data ReloadSpec a
  = RSReloadAll
  | RSReloadList !(HashSet a)
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (ReloadSpec a) where
  toJSON = \case
    RSReloadAll -> Bool True
    RSReloadList l -> toJSON l

instance (FromJSON a, Eq a, Hashable a) => FromJSON (ReloadSpec a) where
  parseJSON (Bool b) = pure $ if b then RSReloadAll else RSReloadList mempty
  parseJSON v = RSReloadList <$> parseJSON v

type ReloadRemoteSchemas = ReloadSpec RemoteSchemaName

type ReloadSources = ReloadSpec SourceName

reloadAllRemoteSchemas :: ReloadRemoteSchemas
reloadAllRemoteSchemas = RSReloadAll

reloadAllSources :: ReloadSources
reloadAllSources = RSReloadAll

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

$(deriveToJSON hasuraJSON ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = withObject "ReloadMetadata" $ \o ->
    ReloadMetadata
      <$> o .:? "reload_remote_schemas" .!= reloadAllRemoteSchemas
      <*> o .:? "reload_sources" .!= reloadAllSources
      <*> o .:? "recreate_event_triggers" .!= RSReloadList mempty

data DumpInternalState
  = DumpInternalState
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''DumpInternalState)

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''GetInconsistentMetadata)

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

data DropInconsistentMetadata
  = DropInconsistentMetadata
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''DropInconsistentMetadata)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

data AllowInconsistentMetadata
  = AllowInconsistentMetadata
  | NoAllowInconsistentMetadata
  deriving (Show, Eq)

instance FromJSON AllowInconsistentMetadata where
  parseJSON =
    withBool "AllowInconsistentMetadata" $
      pure . bool NoAllowInconsistentMetadata AllowInconsistentMetadata

instance ToJSON AllowInconsistentMetadata where
  toJSON = toJSON . toBool
    where
      toBool AllowInconsistentMetadata = True
      toBool NoAllowInconsistentMetadata = False

data ReplaceMetadataV1
  = RMWithSources !Metadata
  | RMWithoutSources !MetadataNoSources
  deriving (Eq)

instance FromJSON ReplaceMetadataV1 where
  parseJSON = withObject "ReplaceMetadataV1" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    case version of
      MVVersion3 -> RMWithSources <$> parseJSON (Object o)
      _ -> RMWithoutSources <$> parseJSON (Object o)

instance ToJSON ReplaceMetadataV1 where
  toJSON = \case
    RMWithSources v -> toJSON v
    RMWithoutSources v -> toJSON v

data ReplaceMetadataV2 = ReplaceMetadataV2
  { _rmv2AllowInconsistentMetadata :: !AllowInconsistentMetadata,
    _rmv2Metadata :: !ReplaceMetadataV1
  }
  deriving (Eq)

instance FromJSON ReplaceMetadataV2 where
  parseJSON = withObject "ReplaceMetadataV2" $ \o ->
    ReplaceMetadataV2
      <$> o .:? "allow_inconsistent_metadata" .!= NoAllowInconsistentMetadata
      <*> o .: "metadata"

instance ToJSON ReplaceMetadataV2 where
  toJSON ReplaceMetadataV2 {..} =
    object
      [ "allow_inconsistent_metadata" .= _rmv2AllowInconsistentMetadata,
        "metadata" .= _rmv2Metadata
      ]

-- TODO: If additional API versions are supported in future it would be ideal to include a version field
--       Rather than differentiating on the "metadata" field.
data ReplaceMetadata
  = RMReplaceMetadataV1 !ReplaceMetadataV1
  | RMReplaceMetadataV2 !ReplaceMetadataV2
  deriving (Eq)

instance FromJSON ReplaceMetadata where
  parseJSON = withObject "ReplaceMetadata" $ \o -> do
    if H.member "metadata" o
      then RMReplaceMetadataV2 <$> parseJSON (Object o)
      else RMReplaceMetadataV1 <$> parseJSON (Object o)

instance ToJSON ReplaceMetadata where
  toJSON = \case
    RMReplaceMetadataV1 v1 -> toJSON v1
    RMReplaceMetadataV2 v2 -> toJSON v2

data WebHookUrl = EnvVar String | URL T.Text
  deriving (Eq)

instance FromJSON WebHookUrl where
  parseJSON (Object o) = do
    var <- o .: "from_env"
    pure $ EnvVar var
  parseJSON (String str) = pure $ URL str
  parseJSON _ = empty

instance ToJSON WebHookUrl where
  toJSON (EnvVar var) = object ["from_env" .= var]
  toJSON (URL url) = toJSON url

data TestWebhookTransform = TestWebhookTransform
  { _twtEnv :: Env.Environment,
    _twtHeaders :: [HTTP.Header],
    _twtWebhookUrl :: WebHookUrl,
    _twtPayload :: Value,
    _twtTransformer :: RequestTransform,
    _twtResponseTransformer :: !(Maybe MetadataResponseTransform),
    _twtSessionVariables :: Maybe SessionVariables
  }
  deriving (Eq)

instance FromJSON TestWebhookTransform where
  parseJSON = withObject "TestWebhookTransform" $ \o -> do
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
    object
      [ "env" .= env,
        "request_headers" .= fmap (first CI.original) headers,
        "webhook_url" .= url,
        "body" .= payload,
        "request_transform" .= mt,
        "response_transform" .= mrt,
        "session_variables" .= sv
      ]
