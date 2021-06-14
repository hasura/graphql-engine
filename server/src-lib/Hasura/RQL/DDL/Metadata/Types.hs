-- | Types related to metadata management API

module Hasura.RQL.DDL.Metadata.Types
  ( currentMetadataVersion
  , MetadataVersion(..)
  , ExportMetadata(..)
  , ClearMetadata(..)
  , ReloadSpec(..)
  , ReloadMetadata(..)
  , DumpInternalState(..)
  , GetInconsistentMetadata(..)
  , DropInconsistentMetadata(..)
  , ReplaceMetadata(..)
  , ReplaceMetadataV1(..)
  , ReplaceMetadataV2(..)
  , AllowInconsistentMetadata(..)
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.TH

import           Hasura.RQL.Types

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
    RSReloadAll    -> Bool True
    RSReloadList l -> toJSON l

instance (FromJSON a, Eq a, Hashable a) => FromJSON (ReloadSpec a) where
  parseJSON (Bool b) = pure $ if b then RSReloadAll else RSReloadList mempty
  parseJSON v        = RSReloadList <$> parseJSON v

type ReloadRemoteSchemas = ReloadSpec RemoteSchemaName
type ReloadSources       = ReloadSpec SourceName

noReloadRemoteSchemas :: ReloadRemoteSchemas
noReloadRemoteSchemas = RSReloadList mempty

reloadAllSources :: ReloadSources
reloadAllSources = RSReloadAll

data ReloadMetadata
  = ReloadMetadata
  { _rmReloadRemoteSchemas :: !ReloadRemoteSchemas
  , _rmReloadSources       :: !ReloadSources
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = \case
    Object o -> ReloadMetadata
                -- To maintain backwards compatibility of the API behaviour,
                -- we choose not to reload any remote schema in absence of
                -- 'reload_remote_schemas' field.
                <$> o .:? "reload_remote_schemas" .!= noReloadRemoteSchemas
                <*> o .:? "reload_sources" .!= reloadAllSources
    _        -> pure $ ReloadMetadata noReloadRemoteSchemas reloadAllSources

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
 deriving(Show, Eq)
$(deriveToJSON defaultOptions ''DropInconsistentMetadata)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

data AllowInconsistentMetadata
  = AllowInconsistentMetadata
  | NoAllowInconsistentMetadata
  deriving (Show, Eq)

instance FromJSON AllowInconsistentMetadata where
  parseJSON = withBool "AllowInconsistentMetadata" $
    pure . bool NoAllowInconsistentMetadata AllowInconsistentMetadata

instance ToJSON AllowInconsistentMetadata where
  toJSON = toJSON . toBool
    where
      toBool AllowInconsistentMetadata   = True
      toBool NoAllowInconsistentMetadata = False

data ReplaceMetadataV1
  = RMWithSources !Metadata
  | RMWithoutSources !MetadataNoSources
  deriving (Eq)

instance FromJSON ReplaceMetadataV1 where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    case version of
      MVVersion3 -> RMWithSources <$> parseJSON (Object o)
      _          -> RMWithoutSources <$> parseJSON (Object o)

instance ToJSON ReplaceMetadataV1 where
  toJSON = \case
    RMWithSources v    -> toJSON v
    RMWithoutSources v -> toJSON v

data ReplaceMetadataV2
  = ReplaceMetadataV2
  { _rmv2AllowInconsistentMetadata :: !AllowInconsistentMetadata
  , _rmv2Metadata                  :: !ReplaceMetadataV1
  } deriving (Eq)

instance FromJSON ReplaceMetadataV2 where
  parseJSON = withObject "ReplaceMetadataV2" $ \o ->
    ReplaceMetadataV2
      <$> o .:? "allow_inconsistent_metadata" .!= NoAllowInconsistentMetadata
      <*> o .: "metadata"

instance ToJSON ReplaceMetadataV2 where
  toJSON ReplaceMetadataV2{..} = object
    [ "allow_inconsistent_metadata" .= _rmv2AllowInconsistentMetadata
    , "metadata" .= _rmv2Metadata
    ]

data ReplaceMetadata
  = RMReplaceMetadataV1 !ReplaceMetadataV1
  | RMReplaceMetadataV2 !ReplaceMetadataV2
  deriving (Eq)

instance FromJSON ReplaceMetadata where
  parseJSON v = RMReplaceMetadataV2 <$> parseJSON v <|> RMReplaceMetadataV1 <$> parseJSON v

instance ToJSON ReplaceMetadata where
  toJSON = \case
    RMReplaceMetadataV1 v1 -> toJSON v1
    RMReplaceMetadataV2 v2 -> toJSON v2
