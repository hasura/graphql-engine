-- | Types related to metadata management API

{-# LANGUAGE TypeApplications #-}
module Hasura.RQL.DDL.Metadata.Types
  ( currentMetadataVersion
  , MetadataVersion(..)
  , ExportMetadata(..)
  , ClearMetadata(..)
  , ReloadMetadata(..)
  , DumpInternalState(..)
  , GetInconsistentMetadata(..)
  , DropInconsistentMetadata(..)
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

data ReloadMetadata
  = ReloadMetadata
  { _rmReloadRemoteSchemas :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = \case
    Object o -> ReloadMetadata
                <$> o .:? "reload_remote_schemas" .!= False
    _        -> pure $ ReloadMetadata False

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
