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
    TestWebhookTransform (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict qualified as H
import Hasura.Prelude
import Hasura.RQL.DDL.RequestTransform (MetadataTransform)
import Hasura.RQL.Types
import Hasura.Session (SessionVariables)

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
  { _rmReloadRemoteSchemas :: !ReloadRemoteSchemas,
    _rmReloadSources :: !ReloadSources
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = \case
    Object o ->
      ReloadMetadata
        <$> o .:? "reload_remote_schemas" .!= reloadAllRemoteSchemas
        <*> o .:? "reload_sources" .!= reloadAllSources
    _ -> pure $ ReloadMetadata reloadAllRemoteSchemas reloadAllSources

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

data TestWebhookTransform = TestWebhookTransform
  { _twtWebhookUrl :: Text,
    _twtPayload :: Value,
    _twtTransformer :: MetadataTransform,
    _twtSessionVariables :: Maybe SessionVariables
  }
  deriving (Eq)

instance FromJSON TestWebhookTransform where
  parseJSON = withObject "TestWebhookTransform" $ \o -> do
    url <- o .: "webhook_url"
    payload <- o .: "body"
    transformer <- o .: "request_transform"
    sessionVars <- o .:? "session_variables"
    pure $ TestWebhookTransform url payload transformer sessionVars

instance ToJSON TestWebhookTransform where
  toJSON (TestWebhookTransform url payload mt sv) =
    object
      [ "webhook_url" .= url,
        "body" .= payload,
        "request_transform" .= mt,
        "session_variables" .= sv
      ]
