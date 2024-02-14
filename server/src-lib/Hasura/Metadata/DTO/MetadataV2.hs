module Hasura.Metadata.DTO.MetadataV2 (MetadataV2 (..)) where

import Autodocodec (Autodocodec (Autodocodec), HasCodec (codec), object, optionalField, requiredField, (.=))
import Autodocodec.Extended (versionField)
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Hasura.Metadata.DTO.Placeholder (PlaceholderArray, PlaceholderObject)
import Hasura.Prelude

-- | Revision 2 of the Metadata export format. Note that values of the types,
-- 'PlaceholderArray' and 'PlaceholderObject' are placeholders that will
-- eventually be expanded to represent more detail.
data MetadataV2 = MetadataV2
  { metaV2Actions :: Maybe PlaceholderArray,
    metaV2Allowlist :: Maybe PlaceholderArray,
    metaV2CronTriggers :: Maybe PlaceholderArray,
    metaV2CustomTypes :: Maybe PlaceholderObject,
    metaV2Functions :: Maybe PlaceholderArray,
    metaV2QueryCollections :: Maybe PlaceholderArray,
    metaV2RemoteSchemas :: Maybe PlaceholderArray,
    metaV2Tables :: PlaceholderArray
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec MetadataV2)

instance HasCodec MetadataV2 where
  codec =
    object "MetadataV2"
      $ MetadataV2
      <$ versionField 2
      <*> optionalField "actions" "action definitions which extend Hasura's schema with custom business logic using custom queries and mutations"
      .= metaV2Actions
        <*> optionalField "allowlist" "safe GraphQL operations - when allow lists are enabled only these operations are allowed"
      .= metaV2Allowlist
        <*> optionalField "cron_triggers" "reliably trigger HTTP endpoints to run custom business logic periodically based on a cron schedule"
      .= metaV2CronTriggers
        <*> optionalField "custom_types" "custom type definitions"
      .= metaV2CustomTypes
        <*> optionalField "functions" "user-defined SQL functions"
      .= metaV2Functions
        <*> optionalField "query_collections" "group queries using query collections"
      .= metaV2QueryCollections
        <*> optionalField "remote_schemas" "merge remote GraphQL schemas and provide a unified GraphQL API"
      .= metaV2RemoteSchemas
        <*> requiredField "tables" "configured database tables"
      .= metaV2Tables
