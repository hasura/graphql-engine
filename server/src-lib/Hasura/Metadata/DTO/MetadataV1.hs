module Hasura.Metadata.DTO.MetadataV1 (MetadataV1 (..)) where

import Autodocodec (Autodocodec (Autodocodec), HasCodec (codec), object, optionalField, requiredField, (.=))
import Autodocodec.Extended (optionalVersionField)
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Hasura.Metadata.DTO.Placeholder (PlaceholderArray)
import Hasura.Prelude

-- | Revision 1 of the Metadata export format. Note that values of the types,
-- 'PlaceholderArray' and 'PlaceholderObject' are placeholders that will
-- eventually be expanded to represent more detail.
data MetadataV1 = MetadataV1
  { metaV1Functions :: Maybe PlaceholderArray,
    metaV1RemoteSchemas :: Maybe PlaceholderArray,
    metaV1Tables :: PlaceholderArray
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec MetadataV1)

instance HasCodec MetadataV1 where
  codec =
    object "MetadataV1"
      $ MetadataV1
      <$ optionalVersionField 1
      <*> optionalField "functions" "user-defined SQL functions"
      .= metaV1Functions
        <*> optionalField "remote_schemas" "merge remote GraphQL schemas and provide a unified GraphQL API"
      .= metaV1RemoteSchemas
        <*> requiredField "tables" "configured database tables"
      .= metaV1Tables
