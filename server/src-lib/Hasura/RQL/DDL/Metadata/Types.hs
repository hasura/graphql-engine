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
import           Language.Haskell.TH.Syntax   (Lift)

import qualified Hasura.RQL.DDL.ComputedField as ComputedField

import           Hasura.RQL.Types


data ComputedFieldMeta
  = ComputedFieldMeta
  { _cfmName       :: !ComputedFieldName
  , _cfmDefinition :: !ComputedField.ComputedFieldDefinition
  , _cfmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldMeta)

data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

data ExportMetadata
  = ExportMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''ExportMetadata)

instance FromJSON ExportMetadata where
  parseJSON _ = return ExportMetadata

data ReloadMetadata
  = ReloadMetadata
  { _rmReloadRemoteSchemas :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 3 snakeCase) ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = \case
    Object o -> ReloadMetadata
                <$> o .:? "reload_remote_schemas" .!= False
    _        -> pure $ ReloadMetadata False

data DumpInternalState
  = DumpInternalState
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''DumpInternalState)

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''GetInconsistentMetadata)

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

data DropInconsistentMetadata
 = DropInconsistentMetadata
 deriving(Show, Eq, Lift)
$(deriveToJSON defaultOptions ''DropInconsistentMetadata)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata
