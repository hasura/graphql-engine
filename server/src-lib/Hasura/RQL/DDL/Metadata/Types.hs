-- | Types related to metadata management API

{-# LANGUAGE TypeApplications #-}
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
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.RQL.Types

data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq)
$(deriveToJSON defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

data ExportMetadata
  = ExportMetadata
  deriving (Show, Eq)
$(deriveToJSON defaultOptions ''ExportMetadata)

instance FromJSON ExportMetadata where
  parseJSON _ = return ExportMetadata

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
$(deriveToJSON (aesonDrop 3 snakeCase) ''ReloadMetadata)

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

data ReplaceMetadata
  = RMWithSources !Metadata
  | RMWithoutSources !MetadataNoSources
  deriving (Show, Eq)

instance FromJSON ReplaceMetadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    case version of
      MVVersion3 -> RMWithSources <$> parseJSON (Object o)
      _          -> RMWithoutSources <$> parseJSON (Object o)

instance ToJSON ReplaceMetadata where
  toJSON = \case
    RMWithSources v -> toJSON v
    RMWithoutSources v -> toJSON v
