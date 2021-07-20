module Hasura.RQL.Types.Metadata.Object where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended        as M

import           Control.Lens                        hiding (set, (.=))
import           Data.Aeson.Types
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Endpoint
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Instances          ()
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.Session


data TableMetadataObjId
  = MTORel !RelName !RelType
  | MTOComputedField !ComputedFieldName
  | MTOPerm !RoleName !PermType
  | MTOTrigger !TriggerName
  | MTORemoteRelationship !RemoteRelationshipName
  deriving (Show, Eq, Generic)
instance Hashable TableMetadataObjId

data SourceMetadataObjId b
  = SMOTable !(TableName b)
  | SMOFunction !(FunctionName b)
  | SMOFunctionPermission !(FunctionName b) !RoleName
  | SMOTableObj !(TableName b) !TableMetadataObjId
  deriving (Generic)
deriving instance (Backend b) => Show (SourceMetadataObjId b)
deriving instance (Backend b) => Eq (SourceMetadataObjId b)
instance (Backend b) => Hashable (SourceMetadataObjId b)

-- See Note [Existentially Quantified Types]
data MetadataObjId
  = MOSource !SourceName
  | MOSourceObjId !SourceName !(AB.AnyBackend SourceMetadataObjId)
  | MORemoteSchema !RemoteSchemaName
  -- ^ Originates from user-defined '_arsqName'
  | MORemoteSchemaPermissions !RemoteSchemaName !RoleName
  | MOCustomTypes
  | MOAction !ActionName
  | MOActionPermission !ActionName !RoleName
  | MOCronTrigger !TriggerName
  | MOInheritedRole !RoleName
  | MOEndpoint !EndpointName
  deriving (Generic)
$(makePrisms ''MetadataObjId)
deriving instance Eq MetadataObjId
instance Hashable MetadataObjId

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOSource _                    -> "source"
  MOSourceObjId _ exists        -> AB.dispatchAnyBackend @Backend exists handleSourceObj
  MORemoteSchema _              -> "remote_schema"
  MORemoteSchemaPermissions _ _ -> "remote_schema_permission"
  MOCronTrigger _               -> "cron_trigger"
  MOCustomTypes                 -> "custom_types"
  MOAction _                    -> "action"
  MOActionPermission _ _        -> "action_permission"
  MOInheritedRole _             -> "inherited_role"
  MOEndpoint _                  -> "endpoint"
 where
    handleSourceObj :: forall b. SourceMetadataObjId b -> Text
    handleSourceObj = \case
      SMOTable _  -> "table"
      SMOFunction _ -> "function"
      SMOFunctionPermission _ _ -> "function_permission"
      SMOTableObj _ tableObjectId -> case tableObjectId of
        MTORel _ relType        -> relTypeToTxt relType <> "_relation"
        MTOPerm _ permType      -> permTypeToCode permType <> "_permission"
        MTOTrigger _            -> "event_trigger"
        MTOComputedField _      -> "computed_field"
        MTORemoteRelationship _ -> "remote_relationship"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOSource name -> toTxt name
  MOSourceObjId source exists -> AB.dispatchAnyBackend @Backend exists (handleSourceObj source)
  MORemoteSchema name -> toTxt name
  MORemoteSchemaPermissions name roleName ->
    toTxt roleName <> " permission in remote schema " <> toTxt name
  MOCronTrigger name -> toTxt name
  MOCustomTypes -> "custom_types"
  MOAction name -> toTxt name
  MOActionPermission name roleName -> toTxt roleName <> " permission in " <> toTxt name
  MOInheritedRole inheritedRoleName -> "inherited role " <> toTxt inheritedRoleName
  MOEndpoint name -> toTxt name
  where
    handleSourceObj
      :: forall b
       . Backend b
      => SourceName
      -> SourceMetadataObjId b
      -> Text
    handleSourceObj source = \case
      SMOTable name -> toTxt name <> " in source " <> toTxt source
      SMOFunction name -> toTxt name <> " in source " <> toTxt source
      SMOFunctionPermission functionName roleName ->
        toTxt roleName <> " permission for function "
        <> toTxt functionName <> " in source " <> toTxt source
      SMOTableObj tableName tableObjectId ->
        let tableObjectName = case tableObjectId of
              MTORel name _              -> toTxt name
              MTOComputedField name      -> toTxt name
              MTORemoteRelationship name -> toTxt name
              MTOPerm name _             -> toTxt name
              MTOTrigger name            -> toTxt name
        in tableObjectName
            <> " in "
            <> moiName (MOSourceObjId source
                         $ AB.mkAnyBackend
                         $ SMOTable @b tableName)

data MetadataObject
  = MetadataObject
  { _moId         :: !MetadataObjId
  , _moDefinition :: !Value
  } deriving (Eq)
$(makeLenses ''MetadataObject)

data InconsistentMetadata
  = InconsistentObject !Text !(Maybe Value) !MetadataObject
  | ConflictingObjects !Text ![MetadataObject]
  | DuplicateObjects !MetadataObjId ![Value]
  | DuplicateRestVariables !Text !MetadataObject
  | InvalidRestSegments !Text !MetadataObject
  | AmbiguousRestEndpoints !Text ![MetadataObject]
  deriving (Eq)
$(makePrisms ''InconsistentMetadata)

getInconsistentRemoteSchemas :: [InconsistentMetadata] -> [RemoteSchemaName]
getInconsistentRemoteSchemas =
  toListOf (traverse._InconsistentObject._3.moId._MORemoteSchema)

imObjectIds :: InconsistentMetadata -> [MetadataObjId]
imObjectIds = \case
  InconsistentObject _ _ metadata -> [_moId metadata]
  ConflictingObjects _ metadatas  -> map _moId metadatas
  DuplicateObjects objectId _     -> [objectId]
  DuplicateRestVariables _ md     -> [_moId md]
  InvalidRestSegments _ md        -> [_moId md]
  AmbiguousRestEndpoints _ mds    -> take 1 $ map _moId mds -- TODO: Take 1 is a workaround to ensure that conflicts are not reported multiple times per endpoint.

imReason :: InconsistentMetadata -> Text
imReason = \case
  InconsistentObject reason _ _   -> "Inconsistent object: " <> reason
  ConflictingObjects reason _     -> "Conflicting objects: " <> reason
  DuplicateObjects objectId _     -> "Multiple definitions for: " <> moiName objectId
  DuplicateRestVariables reason _ -> "Duplicate variables found in endpoint path: " <> reason
  InvalidRestSegments reason _    -> "Empty segments or unnamed variables are not allowed: " <> reason
  AmbiguousRestEndpoints reason _ -> "Ambiguous URL paths: " <> reason

-- | Builds a map from each unique metadata object id to the inconsistencies associated with it.
-- Note that a single inconsistency can involve multiple metadata objects, so the same inconsistency
-- may appear in the resulting map multiple times!
groupInconsistentMetadataById
  :: [InconsistentMetadata] -> HashMap MetadataObjId (NonEmpty InconsistentMetadata)
groupInconsistentMetadataById = M.fromListWith (<>) . concatMap \metadata ->
  map (, metadata :| []) (imObjectIds metadata)

instance ToJSON InconsistentMetadata where
  toJSON inconsistentMetadata = object (("reason" .= imReason inconsistentMetadata) : extraFields)
    where
      extraFields = case inconsistentMetadata of
        InconsistentObject _ message metadata -> metadataObjectFields message metadata
        ConflictingObjects _ metadatas ->
          [ "objects" .= map (object . metadataObjectFields Nothing) metadatas ]
        DuplicateObjects objectId definitions ->
          [ "type" .= String (moiTypeName objectId)
          , "definitions" .= definitions ]

        DuplicateRestVariables _ md  -> metadataObjectFields Nothing md
        InvalidRestSegments _ md     -> metadataObjectFields Nothing md
        AmbiguousRestEndpoints _ mds -> [ "conflicts" .= map _moDefinition mds ]

      metadataObjectFields (maybeMessage :: Maybe Value) (MetadataObject objectId definition) =
        [ "type" .= String (moiTypeName objectId)
        , "name" .= String (moiName objectId)
        , "definition" .= definition ]
        <> maybe [] (\message -> ["message" .= message]) maybeMessage
