module Hasura.RemoteSchema.Metadata.Permission
  ( RemoteSchemaPermissionDefinition (..),
    RemoteSchemaPermissionMetadata (..),
  )
where

import Autodocodec (HasCodec (codec), object, optionalField', requiredField', requiredFieldWith, (.=))
import Autodocodec.Extended (graphQLSchemaDocumentCodec)
import Data.Aeson qualified as J
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName)
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as TB

newtype RemoteSchemaPermissionDefinition = RemoteSchemaPermissionDefinition
  { _rspdSchema :: G.SchemaDocument
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaPermissionDefinition

instance Hashable RemoteSchemaPermissionDefinition

instance HasCodec RemoteSchemaPermissionDefinition where
  codec =
    object "RemoteSchemaPermissionDefinition"
      $ RemoteSchemaPermissionDefinition
      <$> requiredFieldWith
        "schema"
        graphQLSchemaDocumentCodec
        "GraphQL schema document, e.g. the content of schema.gql"
      .= _rspdSchema

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

instance HasCodec RemoteSchemaPermissionMetadata where
  codec =
    object "RemoteSchemaPermissionMetadata"
      $ RemoteSchemaPermissionMetadata
      <$> requiredField' "role"
      .= _rspmRole
        <*> requiredField' "definition"
      .= _rspmDefinition
        <*> optionalField' "comment"
      .= _rspmComment

instance J.FromJSON RemoteSchemaPermissionMetadata where
  parseJSON = J.genericParseJSON hasuraJSON {J.omitNothingFields = True}

instance J.ToJSON RemoteSchemaPermissionMetadata where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}
