{-# LANGUAGE OverloadedLists #-}

module Hasura.Metadata.DTO.MetadataV3 (MetadataV3 (..)) where

import Autodocodec
  ( Autodocodec (Autodocodec),
    HasCodec (codec),
    object,
    optionalField,
    optionalFieldWithOmittedDefault,
    optionalFieldWithOmittedDefaultWith,
    requiredFieldWith,
    (.=),
  )
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.OpenApi qualified as OpenApi
import Hasura.Metadata.DTO.Placeholder (PlaceholderArray, PlaceholderObject)
import Hasura.Metadata.DTO.Utils (versionField)
import Hasura.Prelude
import Hasura.RQL.Types.Action (ActionMetadata (_amName))
import Hasura.RQL.Types.CustomTypes (CustomTypes, emptyCustomTypes)
import Hasura.RQL.Types.Endpoint (_ceName)
import Hasura.RQL.Types.Metadata.Common (Actions, CronTriggers, Endpoints, QueryCollections, RemoteSchemas, Sources, sourcesCodec)
import Hasura.RQL.Types.QueryCollection qualified as QC
import Hasura.RQL.Types.ScheduledTrigger (CronTriggerMetadata (ctName))
import Hasura.RemoteSchema.Metadata.Core (RemoteSchemaMetadataG (_rsmName))

-- | Revision 3 of the Metadata export format. Note that values of the types,
-- 'PlaceholderArray' and 'PlaceholderObject' will eventually be expanded to represent more detail.
data MetadataV3 = MetadataV3
  { metaV3Sources :: Sources,
    metaV3RemoteSchemas :: RemoteSchemas,
    metaV3QueryCollections :: QueryCollections,
    metaV3Allowlist :: Maybe PlaceholderArray,
    metaV3Actions :: Actions,
    metaV3CustomTypes :: CustomTypes,
    metaV3CronTriggers :: CronTriggers,
    metaV3RestEndpoints :: Endpoints,
    metaV3ApiLimits :: Maybe PlaceholderObject,
    metaV3MetricsConfig :: Maybe PlaceholderObject,
    metaV3InheritedRoles :: Maybe PlaceholderArray,
    metaV3GraphqlSchemaIntrospection :: Maybe PlaceholderObject,
    metaV3Network :: Maybe PlaceholderObject,
    metaV3BackendConfigs :: Maybe PlaceholderObject,
    metaV3OpenTelemetryConfig :: Maybe PlaceholderObject
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec MetadataV3)

-- | Codecs simultaneously provide serialization logic for a type, and
-- documentation. A codec can be used to generate a specification in OpenAPI or
-- another format that matches the JSON serialization of the same type.
-- Documentation strings (the second argument to 'optionalField' and to
-- 'requiredField') appear in the generated specification for users' reference.
instance HasCodec MetadataV3 where
  codec =
    object "MetadataV3" $
      MetadataV3
        <$ versionField 3
        <*> requiredFieldWith "sources" sourcesCodec "configured databases" .= metaV3Sources
        <*> optionalFieldWithOmittedDefaultWith "remote_schemas" (sortedElemsCodec _rsmName) [] "merge remote GraphQL schemas and provide a unified GraphQL API" .= metaV3RemoteSchemas
        <*> optionalFieldWithOmittedDefaultWith
          "query_collections"
          (sortedElemsCodec QC._ccName)
          mempty
          "group queries using query collections"
          .= metaV3QueryCollections
        <*> optionalField "allowlist" "safe GraphQL operations - when allow lists are enabled only these operations are allowed" .= metaV3Allowlist
        <*> optionalFieldWithOmittedDefaultWith "actions" (sortedElemsCodec _amName) mempty "action definitions which extend Hasura's schema with custom business logic using custom queries and mutations" .= metaV3Actions
        <*> optionalFieldWithOmittedDefault "custom_types" emptyCustomTypes "custom type definitions" .= metaV3CustomTypes
        <*> optionalFieldWithOmittedDefaultWith "cron_triggers" (sortedElemsCodec ctName) [] "reliably trigger HTTP endpoints to run custom business logic periodically based on a cron schedule" .= metaV3CronTriggers
        <*> optionalFieldWithOmittedDefaultWith "rest_endpoints" (sortedElemsCodec _ceName) [] "REST interfaces to saved GraphQL queries and mutations" .= metaV3RestEndpoints
        <*> optionalField "api_limits" "limts to depth and/or rate of API requests" .= metaV3ApiLimits
        <*> optionalField "metrics_config" "TODO" .= metaV3MetricsConfig
        <*> optionalField "inherited_roles" "an inherited role is a way to create a new role which inherits permissions from two or more roles" .= metaV3InheritedRoles
        <*> optionalField "graphql_schema_introspection" "TODO" .= metaV3GraphqlSchemaIntrospection
        <*> optionalField "network" "TODO" .= metaV3Network
        <*> optionalField "backend_configs" "TODO" .= metaV3BackendConfigs
        <*> optionalField "opentelemetry" "TODO" .= metaV3OpenTelemetryConfig
