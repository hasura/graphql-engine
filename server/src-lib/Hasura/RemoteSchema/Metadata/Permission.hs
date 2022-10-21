{-# LANGUAGE TemplateHaskell #-}

module Hasura.RemoteSchema.Metadata.Permission
  ( RemoteSchemaPermissionDefinition (..),
    RemoteSchemaPermissionMetadata (..),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.Session
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as TB

newtype RemoteSchemaPermissionDefinition = RemoteSchemaPermissionDefinition
  { _rspdSchema :: G.SchemaDocument
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaPermissionDefinition

instance Cacheable RemoteSchemaPermissionDefinition

instance Hashable RemoteSchemaPermissionDefinition

instance J.FromJSON RemoteSchemaPermissionDefinition where
  parseJSON = J.withObject "RemoteSchemaPermissionDefinition" $ \obj -> do
    fmap RemoteSchemaPermissionDefinition $ obj J..: "schema"

instance J.ToJSON RemoteSchemaPermissionDefinition where
  toJSON (RemoteSchemaPermissionDefinition schema) =
    J.object $ ["schema" J..= J.String (TB.run . G.schemaDocument $ schema)]

data RemoteSchemaPermissionMetadata = RemoteSchemaPermissionMetadata
  { _rspmRole :: RoleName,
    _rspmDefinition :: RemoteSchemaPermissionDefinition,
    _rspmComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Cacheable RemoteSchemaPermissionMetadata

$(J.deriveJSON hasuraJSON {J.omitNothingFields = True} ''RemoteSchemaPermissionMetadata)
