{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Metadata.Object
  ( InconsistentMetadata (..),
    InconsistentRoleEntity (..),
    MetadataObjId (..),
    MetadataObject (..),
    SourceMetadataObjId (..),
    TableMetadataObjId (..),
    LogicalModelMetadataObjId (..),
    NativeQueryMetadataObjId (..),
    droppableInconsistentMetadata,
    getInconsistentRemoteSchemas,
    groupInconsistentMetadataById,
    imObjectIds,
    imReason,
    moDefinition,
    moId,
    moiName,
    moiTypeName,
    _AmbiguousRestEndpoints,
    _ConflictingInheritedPermission,
    _ConflictingObjects,
    _DuplicateObjects,
    _DuplicateRestVariables,
    _InconsistentObject,
    _InvalidRestSegments,
    _MOAction,
    _MOActionPermission,
    _MOCronTrigger,
    _MOCustomTypes,
    _MOEndpoint,
    _MOInheritedRole,
    _MORemoteSchema,
    _MORemoteSchemaPermissions,
    _MOSource,
    _MOSourceObjId,
  )
where

import Control.Lens hiding (set, (.=))
import Data.Aeson.Types
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Text.Extended
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorName)
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue
import Hasura.LogicalModel.Types
import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection (CollectionName, ListedQuery (_lqName))
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

data TableMetadataObjId
  = MTORel RelName RelType
  | MTOComputedField ComputedFieldName
  | MTOPerm RoleName PermType
  | MTOTrigger TriggerName
  | MTORemoteRelationship RelName
  deriving (Show, Eq, Ord, Generic)

instance Hashable TableMetadataObjId

-- | Identifiers for logical model elements within the metadata structure.
data LogicalModelMetadataObjId
  = LMMOPerm RoleName PermType
  | LMMOReferencedLogicalModel LogicalModelName
  deriving (Show, Eq, Ord, Generic)

instance Hashable LogicalModelMetadataObjId

-- | the native query should probably also link to its logical model
data NativeQueryMetadataObjId
  = NQMORel RelName RelType
  deriving (Show, Eq, Ord, Generic)

instance Hashable NativeQueryMetadataObjId

data SourceMetadataObjId b
  = SMOTable (TableName b)
  | SMOFunction (FunctionName b)
  | SMOFunctionPermission (FunctionName b) RoleName
  | SMOTableObj (TableName b) TableMetadataObjId
  | SMONativeQuery NativeQueryName
  | SMONativeQueryObj NativeQueryName NativeQueryMetadataObjId
  | SMOStoredProcedure (FunctionName b)
  | SMOLogicalModel LogicalModelName
  | SMOLogicalModelObj LogicalModelLocation LogicalModelMetadataObjId
  deriving (Generic)

deriving instance (Backend b) => Show (SourceMetadataObjId b)

deriving instance (Backend b) => Eq (SourceMetadataObjId b)

deriving instance (Backend b) => Ord (SourceMetadataObjId b)

instance (Backend b) => Hashable (SourceMetadataObjId b)

data MetadataObjId
  = MOSource SourceName
  | MOSourceObjId SourceName (AB.AnyBackend SourceMetadataObjId)
  | -- | Originates from user-defined '_arsqName'
    MORemoteSchema RemoteSchemaName
  | MORemoteSchemaPermissions RemoteSchemaName RoleName
  | -- | A remote relationship on a remote schema type, identified by
    -- 1. remote schema name
    -- 2. remote schema type on which the relationship is defined
    -- 3. name of the relationship
    MORemoteSchemaRemoteRelationship RemoteSchemaName G.Name RelName
  | MOCustomTypes
  | MOAction ActionName
  | MOActionPermission ActionName RoleName
  | MOCronTrigger TriggerName
  | MOInheritedRole RoleName
  | MOEndpoint EndpointName
  | MOQueryCollectionsQuery CollectionName ListedQuery
  | MODataConnectorAgent DataConnectorName
  | MOOpenTelemetry OpenTelemetryConfigSubobject
  deriving (Show, Eq, Ord, Generic)

$(makePrisms ''MetadataObjId)

instance Hashable MetadataObjId

instance ToErrorValue MetadataObjId where
  toErrorValue = toErrorMessage . moiName

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOSource _ -> "source"
  MOSourceObjId _ exists -> AB.dispatchAnyBackend @Backend exists handleSourceObj
  MORemoteSchema _ -> "remote_schema"
  MORemoteSchemaPermissions _ _ -> "remote_schema_permission"
  MORemoteSchemaRemoteRelationship {} -> "remote_schema_remote_relationship"
  MOCronTrigger _ -> "cron_trigger"
  MOCustomTypes -> "custom_types"
  MOAction _ -> "action"
  MOActionPermission _ _ -> "action_permission"
  MOInheritedRole _ -> "inherited_role"
  MOEndpoint _ -> "rest_endpoint"
  MOQueryCollectionsQuery _ _ -> "query_collections"
  MODataConnectorAgent _ -> "data_connector_agent"
  MOOpenTelemetry _ -> "open_telemetry"
  where
    handleSourceObj :: forall b. SourceMetadataObjId b -> Text
    handleSourceObj = \case
      SMOTable _ -> "table"
      SMOFunction _ -> "function"
      SMONativeQuery _ -> "native_query"
      SMONativeQueryObj _ nativeQueryObjId -> case nativeQueryObjId of
        NQMORel _ relType -> relTypeToTxt relType <> "_relation"
      SMOStoredProcedure _ -> "stored_procedure"
      SMOLogicalModel _ -> "logical_model"
      SMOLogicalModelObj _ logicalModelObjectId -> case logicalModelObjectId of
        LMMOPerm _ permType -> permTypeToCode permType <> "_permission"
        LMMOReferencedLogicalModel name -> "inner_logical_model_" <> toTxt name
      SMOFunctionPermission _ _ -> "function_permission"
      SMOTableObj _ tableObjectId -> case tableObjectId of
        MTORel _ relType -> relTypeToTxt relType <> "_relation"
        MTOPerm _ permType -> permTypeToCode permType <> "_permission"
        MTOTrigger _ -> "event_trigger"
        MTOComputedField _ -> "computed_field"
        MTORemoteRelationship _ -> "remote_relationship"

moiName :: MetadataObjId -> Text
moiName objectId =
  moiTypeName objectId <> " " <> case objectId of
    MOSource name -> toTxt name
    MOSourceObjId source exists -> AB.dispatchAnyBackend @Backend exists (handleSourceObj source)
    MORemoteSchema name -> toTxt name
    MORemoteSchemaPermissions name roleName ->
      toTxt roleName <> " permission in remote schema " <> toTxt name
    MORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName ->
      "remote_relationship "
        <> toTxt relationshipName
        <> " on type "
        <> G.unName typeName
        <> " in remote schema "
        <> toTxt remoteSchemaName
    MOCronTrigger name -> toTxt name
    MOCustomTypes -> "custom_types"
    MOAction name -> toTxt name
    MOActionPermission name roleName -> toTxt roleName <> " permission in " <> toTxt name
    MOInheritedRole inheritedRoleName -> "inherited role " <> toTxt inheritedRoleName
    MOEndpoint name -> toTxt name
    MOQueryCollectionsQuery cName lq -> (toTxt . _lqName) lq <> " in " <> toTxt cName
    MODataConnectorAgent agentName -> toTxt agentName
    MOOpenTelemetry subobject -> case subobject of
      OtelSubobjectAll -> "all"
      OtelSubobjectExporterOtlp -> "exporter_otlp"
      OtelSubobjectBatchSpanProcessor -> "batch_span_processor"
  where
    handleSourceObj ::
      forall b.
      (Backend b) =>
      SourceName ->
      SourceMetadataObjId b ->
      Text
    handleSourceObj source = \case
      SMOTable name -> toTxt name <> " in source " <> toTxt source
      SMOFunction name -> toTxt name <> " in source " <> toTxt source
      SMOFunctionPermission functionName roleName ->
        toTxt roleName
          <> " permission for function "
          <> toTxt functionName
          <> " in source "
          <> toTxt source
      SMONativeQuery name -> toTxt name <> " in source " <> toTxt source
      SMONativeQueryObj nativeQueryName nativeQueryObjId ->
        case nativeQueryObjId of
          NQMORel name _ -> toTxt name <> " in " <> toTxt nativeQueryName
      SMOStoredProcedure name -> toTxt name <> " in source " <> toTxt source
      SMOLogicalModel name -> toTxt name <> " in source " <> toTxt source
      SMOLogicalModelObj (LMLLogicalModel logicalModelName) logicalModelObjectId -> do
        let objectName :: Text
            objectName = case logicalModelObjectId of
              LMMOPerm name _ -> toTxt name
              LMMOReferencedLogicalModel name -> toTxt name

            sourceObjectId :: MetadataObjId
            sourceObjectId =
              MOSourceObjId source
                $ AB.mkAnyBackend
                $ SMOLogicalModel @b logicalModelName

        objectName <> " in " <> moiName sourceObjectId
      SMOLogicalModelObj (LMLNativeQuery nativeQueryName) logicalModelObjectId -> do
        let objectName :: Text
            objectName = case logicalModelObjectId of
              LMMOPerm name _ -> toTxt name
              LMMOReferencedLogicalModel name -> toTxt name

            sourceObjectId :: MetadataObjId
            sourceObjectId =
              MOSourceObjId source
                $ AB.mkAnyBackend
                $ SMONativeQuery @b nativeQueryName

        objectName <> " in " <> moiName sourceObjectId
      SMOTableObj tableName tableObjectId ->
        let tableObjectName = case tableObjectId of
              MTORel name _ -> toTxt name
              MTOComputedField name -> toTxt name
              MTORemoteRelationship name -> toTxt name
              MTOPerm name _ -> toTxt name
              MTOTrigger name -> toTxt name
         in tableObjectName
              <> " in "
              <> moiName
                ( MOSourceObjId source
                    $ AB.mkAnyBackend
                    $ SMOTable @b tableName
                )

data MetadataObject = MetadataObject
  { _moId :: MetadataObjId,
    _moDefinition :: Value
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable MetadataObject

$(makeLenses ''MetadataObject)

data InconsistentRoleEntity
  = InconsistentTablePermission
      SourceName
      -- | Table name -- using `Text` here instead of `TableName b` for simplification,
      -- Otherwise, we'll have to create a newtype wrapper around `TableName b` and then
      -- use it with `AB.AnyBackend`
      Text
      PermType
  | InconsistentRemoteSchemaPermission RemoteSchemaName
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable InconsistentRoleEntity

instance ToTxt InconsistentRoleEntity where
  toTxt (InconsistentTablePermission source table permType) =
    permTypeToCode permType
      <> " permission"
      <> (", table: " :: Text)
      <> table
      <> ", source: "
      <> squote source
  toTxt (InconsistentRemoteSchemaPermission remoteSchemaName) =
    "remote schema: " <> squote remoteSchemaName

instance ToJSON InconsistentRoleEntity where
  toJSON = \case
    InconsistentTablePermission sourceName tableName permType ->
      object
        [ "table" .= tableName,
          "source" .= sourceName,
          "permission_type" .= permType
        ]
    InconsistentRemoteSchemaPermission remoteSchemaName ->
      object ["remote_schema" .= remoteSchemaName]

data InconsistentMetadata
  = InconsistentObject Text (Maybe Value) MetadataObject
  | ConflictingObjects Text [MetadataObject]
  | DuplicateObjects MetadataObjId [Value]
  | DuplicateRestVariables Text MetadataObject
  | InvalidRestSegments Text MetadataObject
  | AmbiguousRestEndpoints Text [MetadataObject]
  | ConflictingInheritedPermission RoleName InconsistentRoleEntity
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable InconsistentMetadata

$(makePrisms ''InconsistentMetadata)

-- | Helper function to differentiate which type of inconsistent
--   metadata can be dropped, if an inconsistency cannot be resolved
--   by dropping any part of the metadata then this function should
--   return `False`, otherwise it should return `True`
droppableInconsistentMetadata :: InconsistentMetadata -> Bool
droppableInconsistentMetadata = \case
  InconsistentObject {} -> True
  ConflictingObjects {} -> True
  DuplicateObjects {} -> True
  DuplicateRestVariables {} -> True
  InvalidRestSegments {} -> True
  AmbiguousRestEndpoints {} -> True
  ConflictingInheritedPermission {} -> False

getInconsistentRemoteSchemas :: [InconsistentMetadata] -> [RemoteSchemaName]
getInconsistentRemoteSchemas =
  toListOf (traverse . _InconsistentObject . _3 . moId . _MORemoteSchema)

imObjectIds :: InconsistentMetadata -> [MetadataObjId]
imObjectIds = \case
  InconsistentObject _ _ metadata -> [_moId metadata]
  ConflictingObjects _ metadatas -> map _moId metadatas
  DuplicateObjects objectId _ -> [objectId]
  DuplicateRestVariables _ md -> [_moId md]
  InvalidRestSegments _ md -> [_moId md]
  AmbiguousRestEndpoints _ mds -> take 1 $ map _moId mds -- TODO: Take 1 is a workaround to ensure that conflicts are not reported multiple times per endpoint.
  ConflictingInheritedPermission _ _ -> mempty -- @mempty@ because in such a case we just want the user to know that the permission was not able to derive and this inconsistency is purely informational

imReason :: InconsistentMetadata -> Text
imReason = \case
  InconsistentObject reason _ _ -> "Inconsistent object: " <> reason
  ConflictingObjects reason _ -> "Conflicting objects: " <> reason
  DuplicateObjects objectId _ -> "Multiple definitions for: " <> moiName objectId
  DuplicateRestVariables reason _ -> "Duplicate variables found in endpoint path: " <> reason
  InvalidRestSegments reason _ -> "Empty segments or unnamed variables are not allowed: " <> reason
  AmbiguousRestEndpoints reason _ -> "Ambiguous URL paths: " <> reason
  ConflictingInheritedPermission roleName entity ->
    "Could not inherit permission for the role "
      <> squote roleName
      <> (" for the entity: " :: Text)
      <> squote entity

-- | Builds a map from each unique metadata object id to the inconsistencies associated with it.
-- Note that a single inconsistency can involve multiple metadata objects, so the same inconsistency
-- may appear in the resulting map multiple times!
groupInconsistentMetadataById ::
  [InconsistentMetadata] -> HashMap MetadataObjId (NonEmpty InconsistentMetadata)
groupInconsistentMetadataById =
  HashMap.fromListWith (<>) . concatMap \metadata ->
    map (,metadata :| []) (imObjectIds metadata)

instance ToJSON InconsistentMetadata where
  toJSON inconsistentMetadata = object (("reason" .= imReason inconsistentMetadata) : extraFields)
    where
      extraFields = case inconsistentMetadata of
        InconsistentObject _ message metadata -> metadataObjectFields message metadata
        ConflictingObjects _ metadatas ->
          ["objects" .= map (object . metadataObjectFields Nothing) metadatas]
        DuplicateObjects objectId definitions ->
          [ "type" .= String (moiTypeName objectId),
            "definitions" .= definitions
          ]
        DuplicateRestVariables _ md -> metadataObjectFields Nothing md
        InvalidRestSegments _ md -> metadataObjectFields Nothing md
        AmbiguousRestEndpoints _ mds -> ["conflicts" .= map _moDefinition mds]
        ConflictingInheritedPermission role inconsistentEntity ->
          [ "name" .= role,
            "type" .= String ("inherited role permission inconsistency" :: Text),
            "entity" .= inconsistentEntity
          ]

      metadataObjectFields (maybeMessage :: Maybe Value) (MetadataObject objectId definition) =
        [ "type" .= String (moiTypeName objectId),
          "name" .= String (moiName objectId),
          "definition" .= definition
        ]
          <> maybe [] (\message -> ["message" .= message]) maybeMessage
