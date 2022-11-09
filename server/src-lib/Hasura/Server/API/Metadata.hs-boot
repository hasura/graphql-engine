module Hasura.Server.API.Metadata
  ( RQLMetadataV1 (..),
  )
where

import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.DataConnector
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.Metadata
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.QueryTags
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.Relationship.Rename
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Source
import Hasura.RQL.DDL.SourceKinds
import Hasura.RQL.DDL.Webhook.Transform.Validation
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RemoteSchema.MetadataAPI
import Hasura.SQL.AnyBackend

data RQLMetadataV1
  = -- Sources
    RMAddSource !(AnyBackend AddSource)
  | RMDropSource DropSource
  | RMRenameSource !RenameSource
  | RMUpdateSource !(AnyBackend UpdateSource)
  | RMListSourceKinds !ListSourceKinds
  | RMGetSourceKindCapabilities !GetSourceKindCapabilities
  | RMGetSourceTables !GetSourceTables
  | RMGetTableInfo !GetTableInfo
  | -- Tables
    RMTrackTable !(AnyBackend TrackTableV2)
  | RMUntrackTable !(AnyBackend UntrackTable)
  | RMSetTableCustomization !(AnyBackend SetTableCustomization)
  | RMSetApolloFederationConfig (AnyBackend SetApolloFederationConfig)
  | -- Tables (PG-specific)
    RMPgSetTableIsEnum !(AnyBackend SetTableIsEnum)
  | -- Tables permissions
    RMCreateInsertPermission !(AnyBackend (CreatePerm InsPerm))
  | RMCreateSelectPermission !(AnyBackend (CreatePerm SelPerm))
  | RMCreateUpdatePermission !(AnyBackend (CreatePerm UpdPerm))
  | RMCreateDeletePermission !(AnyBackend (CreatePerm DelPerm))
  | RMDropInsertPermission !(AnyBackend DropPerm)
  | RMDropSelectPermission !(AnyBackend DropPerm)
  | RMDropUpdatePermission !(AnyBackend DropPerm)
  | RMDropDeletePermission !(AnyBackend DropPerm)
  | RMSetPermissionComment !(AnyBackend SetPermComment)
  | -- Tables relationships
    RMCreateObjectRelationship !(AnyBackend CreateObjRel)
  | RMCreateArrayRelationship !(AnyBackend CreateArrRel)
  | RMDropRelationship !(AnyBackend DropRel)
  | RMSetRelationshipComment !(AnyBackend SetRelComment)
  | RMRenameRelationship !(AnyBackend RenameRel)
  | -- Tables remote relationships
    RMCreateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMUpdateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMDeleteRemoteRelationship !(AnyBackend DeleteFromSourceRelationship)
  | -- Functions
    RMTrackFunction !(AnyBackend TrackFunctionV2)
  | RMUntrackFunction !(AnyBackend UnTrackFunction)
  | RMSetFunctionCustomization (AnyBackend SetFunctionCustomization)
  | -- Functions permissions
    RMCreateFunctionPermission !(AnyBackend FunctionPermissionArgument)
  | RMDropFunctionPermission !(AnyBackend FunctionPermissionArgument)
  | -- Computed fields
    RMAddComputedField !(AnyBackend AddComputedField)
  | RMDropComputedField !(AnyBackend DropComputedField)
  | -- Tables event triggers
    RMCreateEventTrigger !(AnyBackend (Unvalidated1 CreateEventTriggerQuery))
  | RMDeleteEventTrigger !(AnyBackend DeleteEventTriggerQuery)
  | RMRedeliverEvent !(AnyBackend RedeliverEventQuery)
  | RMInvokeEventTrigger !(AnyBackend InvokeEventTriggerQuery)
  | RMCleanupEventTriggerLog !TriggerLogCleanupConfig
  | RMResumeEventTriggerCleanup !TriggerLogCleanupToggleConfig
  | RMPauseEventTriggerCleanup !TriggerLogCleanupToggleConfig
  | -- Remote schemas
    RMAddRemoteSchema !AddRemoteSchemaQuery
  | RMUpdateRemoteSchema !AddRemoteSchemaQuery
  | RMRemoveRemoteSchema !RemoteSchemaNameQuery
  | RMReloadRemoteSchema !RemoteSchemaNameQuery
  | RMIntrospectRemoteSchema !RemoteSchemaNameQuery
  | -- Remote schemas permissions
    RMAddRemoteSchemaPermissions !AddRemoteSchemaPermission
  | RMDropRemoteSchemaPermissions !DropRemoteSchemaPermissions
  | -- Remote Schema remote relationships
    RMCreateRemoteSchemaRemoteRelationship CreateRemoteSchemaRemoteRelationship
  | RMUpdateRemoteSchemaRemoteRelationship CreateRemoteSchemaRemoteRelationship
  | RMDeleteRemoteSchemaRemoteRelationship DeleteRemoteSchemaRemoteRelationship
  | -- Scheduled triggers
    RMCreateCronTrigger !(Unvalidated CreateCronTrigger)
  | RMDeleteCronTrigger !ScheduledTriggerName
  | RMCreateScheduledEvent !CreateScheduledEvent
  | RMDeleteScheduledEvent !DeleteScheduledEvent
  | RMGetScheduledEvents !GetScheduledEvents
  | RMGetScheduledEventInvocations !GetScheduledEventInvocations
  | RMGetCronTriggers
  | -- Actions
    RMCreateAction !(Unvalidated CreateAction)
  | RMDropAction !DropAction
  | RMUpdateAction !(Unvalidated UpdateAction)
  | RMCreateActionPermission !CreateActionPermission
  | RMDropActionPermission !DropActionPermission
  | -- Query collections, allow list related
    RMCreateQueryCollection !CreateCollection
  | RMRenameQueryCollection !RenameCollection
  | RMDropQueryCollection !DropCollection
  | RMAddQueryToCollection !AddQueryToCollection
  | RMDropQueryFromCollection !DropQueryFromCollection
  | RMAddCollectionToAllowlist !AllowlistEntry
  | RMDropCollectionFromAllowlist !DropCollectionFromAllowlist
  | RMUpdateScopeOfCollectionInAllowlist !UpdateScopeOfCollectionInAllowlist
  | -- Rest endpoints
    RMCreateRestEndpoint !CreateEndpoint
  | RMDropRestEndpoint !DropEndpoint
  | -- GraphQL Data Connectors
    RMDCAddAgent !DCAddAgent
  | RMDCDeleteAgent !DCDeleteAgent
  | -- Custom types
    RMSetCustomTypes !CustomTypes
  | -- Api limits
    RMSetApiLimits !ApiLimit
  | RMRemoveApiLimits
  | -- Metrics config
    RMSetMetricsConfig !MetricsConfig
  | RMRemoveMetricsConfig
  | -- Inherited roles
    RMAddInheritedRole !InheritedRole
  | RMDropInheritedRole !DropInheritedRole
  | -- Metadata management
    RMReplaceMetadata !ReplaceMetadata
  | RMExportMetadata !ExportMetadata
  | RMClearMetadata !ClearMetadata
  | RMReloadMetadata !ReloadMetadata
  | RMGetInconsistentMetadata !GetInconsistentMetadata
  | RMDropInconsistentMetadata !DropInconsistentMetadata
  | -- Introspection options
    RMSetGraphqlSchemaIntrospectionOptions !SetGraphqlIntrospectionOptions
  | -- Network
    RMAddHostToTLSAllowlist !AddHostToTLSAllowlist
  | RMDropHostFromTLSAllowlist !DropHostFromTLSAllowlist
  | -- QueryTags
    RMSetQueryTagsConfig !SetQueryTagsConfig
  | -- Debug
    RMDumpInternalState !DumpInternalState
  | RMGetCatalogState !GetCatalogState
  | RMSetCatalogState !SetCatalogState
  | RMTestWebhookTransform !(Unvalidated TestWebhookTransform)
  | -- Bulk metadata queries
    RMBulk [RQLMetadataRequest]

data RQLMetadataV2
  = RMV2ReplaceMetadata !ReplaceMetadataV2
  | RMV2ExportMetadata !ExportMetadata

data RQLMetadataRequest
  = RMV1 !RQLMetadataV1
  | RMV2 !RQLMetadataV2
