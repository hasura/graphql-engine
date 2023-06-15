-- | The RQL metadata query ('/v1/metadata')
module Hasura.Server.API.Metadata.Types
  ( RQLMetadataV1 (..),
    RQLMetadataV2 (..),
    RQLMetadataRequest (..),
  )
where

import GHC.Generics
import Hasura.Function.API qualified as Functions
import Hasura.LogicalModel.API qualified as LogicalModel
import Hasura.NativeQuery.API qualified as NativeQueries
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.ConnectionTemplate
import Hasura.RQL.DDL.DataConnector
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.Metadata
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.QueryTags
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.Relationship.Rename
import Hasura.RQL.DDL.Relationship.Suggest
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
import Hasura.RQL.Types.Metadata (GetCatalogState, SetCatalogState)
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RemoteSchema.MetadataAPI
import Hasura.SQL.AnyBackend
import Hasura.StoredProcedure.API qualified as StoredProcedures
import Network.Types.Extended

data RQLMetadataV1
  = -- Sources
    RMAddSource !(AnyBackend AddSource)
  | RMDropSource DropSource
  | RMRenameSource !RenameSource
  | RMUpdateSource !(AnyBackend UpdateSource)
  | RMListSourceKinds !ListSourceKinds
  | RMGetSourceKindCapabilities !GetSourceKindCapabilities
  | RMGetSourceTables !(AnyBackend GetSourceTables)
  | RMGetSourceTrackables !(AnyBackend GetSourceTrackables)
  | RMGetTableInfo !(AnyBackend GetTableInfo)
  | RMGetTableInfo_ !GetTableInfo_
  | -- Tables
    RMTrackTable !(AnyBackend TrackTableV2)
  | RMTrackTables !(AnyBackend TrackTables)
  | RMUntrackTable !(AnyBackend UntrackTable)
  | RMUntrackTables !(AnyBackend UntrackTables)
  | RMSetTableCustomization !(AnyBackend SetTableCustomization)
  | RMSetApolloFederationConfig (AnyBackend SetApolloFederationConfig)
  | RMPgSetTableIsEnum !(AnyBackend SetTableIsEnum)
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
  | RMSuggestRelationships !(AnyBackend SuggestRels)
  | -- Tables remote relationships
    RMCreateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMUpdateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMDeleteRemoteRelationship !(AnyBackend DeleteFromSourceRelationship)
  | -- Functions
    RMTrackFunction !(AnyBackend Functions.TrackFunctionV2)
  | RMUntrackFunction !(AnyBackend Functions.UnTrackFunction)
  | RMSetFunctionCustomization (AnyBackend Functions.SetFunctionCustomization)
  | -- Functions permissions
    RMCreateFunctionPermission !(AnyBackend Functions.FunctionPermissionArgument)
  | RMDropFunctionPermission !(AnyBackend Functions.FunctionPermissionArgument)
  | -- Computed fields
    RMAddComputedField !(AnyBackend AddComputedField)
  | RMDropComputedField !(AnyBackend DropComputedField)
  | -- Connection template
    RMTestConnectionTemplate !(AnyBackend TestConnectionTemplate)
  | -- Native Queries
    RMGetNativeQuery !(AnyBackend NativeQueries.GetNativeQuery)
  | RMTrackNativeQuery !(AnyBackend NativeQueries.TrackNativeQuery)
  | RMUntrackNativeQuery !(AnyBackend NativeQueries.UntrackNativeQuery)
  | -- Stored Procedures
    RMGetStoredProcedure !(AnyBackend StoredProcedures.GetStoredProcedure)
  | RMTrackStoredProcedure !(AnyBackend StoredProcedures.TrackStoredProcedure)
  | RMUntrackStoredProcedure !(AnyBackend StoredProcedures.UntrackStoredProcedure)
  | -- Custom types
    RMGetLogicalModel !(AnyBackend LogicalModel.GetLogicalModel)
  | RMTrackLogicalModel !(AnyBackend LogicalModel.TrackLogicalModel)
  | RMUntrackLogicalModel !(AnyBackend LogicalModel.UntrackLogicalModel)
  | RMCreateSelectLogicalModelPermission !(AnyBackend (LogicalModel.CreateLogicalModelPermission SelPerm))
  | RMDropSelectLogicalModelPermission !(AnyBackend LogicalModel.DropLogicalModelPermission)
  | -- Tables event triggers
    RMCreateEventTrigger !(AnyBackend (Unvalidated1 CreateEventTriggerQuery))
  | RMDeleteEventTrigger !(AnyBackend DeleteEventTriggerQuery)
  | RMRedeliverEvent !(AnyBackend RedeliverEventQuery)
  | RMInvokeEventTrigger !(AnyBackend InvokeEventTriggerQuery)
  | RMCleanupEventTriggerLog !TriggerLogCleanupConfig
  | RMResumeEventTriggerCleanup !TriggerLogCleanupToggleConfig
  | RMPauseEventTriggerCleanup !TriggerLogCleanupToggleConfig
  | RMGetEventLogs !(AnyBackend GetEventLogs)
  | RMGetEventInvocationLogs !(AnyBackend GetEventInvocations)
  | RMGetEventById !(AnyBackend GetEventById)
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
  | -- OpenTelemetry
    RMSetOpenTelemetryConfig !OpenTelemetryConfig
  | RMSetOpenTelemetryStatus !OtelStatus
  | -- Debug
    RMDumpInternalState !DumpInternalState
  | RMGetCatalogState !GetCatalogState
  | RMSetCatalogState !SetCatalogState
  | RMTestWebhookTransform !(Unvalidated TestWebhookTransform)
  | -- Bulk metadata queries
    RMBulk [RQLMetadataRequest]
  | -- Bulk metadata queries, but don't stop if something fails - return all
    -- successes and failures as separate items
    RMBulkKeepGoing [RQLMetadataRequest]
  | -- | Bulk metadata queries, running a single schema cache resolve at the
    -- end. Only works for a subset of commands.
    RMBulkAtomic [RQLMetadataRequest]
  deriving (Generic)

data RQLMetadataV2
  = RMV2ReplaceMetadata !ReplaceMetadataV2
  | RMV2ExportMetadata !ExportMetadata
  deriving (Generic)

data RQLMetadataRequest
  = RMV1 !RQLMetadataV1
  | RMV2 !RQLMetadataV2
