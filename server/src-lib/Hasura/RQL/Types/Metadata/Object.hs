module Hasura.RQL.Types.Metadata.Object where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended        as M

import           Control.Lens                        hiding (set, (.=))
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Text.Extended
import           Data.Typeable                       (cast)

import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Endpoint
import           Hasura.RQL.Types.EventTrigger
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

data MetadataObjId
  = MOSource !SourceName
  | forall b. (Backend b) => MOSourceObjId !SourceName !(SourceMetadataObjId b)
  | MORemoteSchema !RemoteSchemaName
  -- ^ Originates from user-defined '_arsqName'
  | MORemoteSchemaPermissions !RemoteSchemaName !RoleName
  | MOCustomTypes
  | MOAction !ActionName
  | MOActionPermission !ActionName !RoleName
  | MOCronTrigger !TriggerName
  | MOEndpoint !EndpointName
$(makePrisms ''MetadataObjId)

instance Hashable MetadataObjId where
  hashWithSalt salt = \case
    MOSource sourceName                    -> hashWithSalt salt sourceName
    MOSourceObjId sourceName sourceObjId   -> hashWithSalt salt (sourceName, sourceObjId)
    MORemoteSchema remoteSchemaName        -> hashWithSalt salt remoteSchemaName
    MORemoteSchemaPermissions remoteSchemaName roleName -> hashWithSalt salt (remoteSchemaName, roleName)
    MOCustomTypes                          -> hashWithSalt salt ()
    MOAction actionName                    -> hashWithSalt salt actionName
    MOActionPermission actionName roleName -> hashWithSalt salt (actionName, roleName)
    MOCronTrigger triggerName              -> hashWithSalt salt triggerName
    MOEndpoint endpoint                    -> hashWithSalt salt endpoint

instance Eq MetadataObjId where
  (MOSource s1) == (MOSource s2)                             = s1 == s2
  (MOSourceObjId s1 id1) == (MOSourceObjId s2 id2)           = s1 == s2 && Just id1 == cast id2
  (MORemoteSchema n1) == (MORemoteSchema n2)                 = n1 == n2
  (MORemoteSchemaPermissions n1 r1) == (MORemoteSchemaPermissions n2 r2) = n1 == n2 && r1 == r2
  MOCustomTypes == MOCustomTypes                             = True
  (MOActionPermission an1 r1) == (MOActionPermission an2 r2) = an1 == an2 && r1 == r2
  (MOCronTrigger trn1) == (MOCronTrigger trn2)               = trn1 == trn2
  _ == _                                                     = False

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOSource _ -> "source"
  MOSourceObjId _ sourceObjId -> case sourceObjId of
    SMOTable _  -> "table"
    SMOFunction _ -> "function"
    SMOFunctionPermission _ _ -> "function_permission"
    SMOTableObj _ tableObjectId -> case tableObjectId of
      MTORel _ relType        -> relTypeToTxt relType <> "_relation"
      MTOPerm _ permType      -> permTypeToCode permType <> "_permission"
      MTOTrigger _            -> "event_trigger"
      MTOComputedField _      -> "computed_field"
      MTORemoteRelationship _ -> "remote_relationship"
  MORemoteSchema _ -> "remote_schema"
  MORemoteSchemaPermissions _ _ -> "remote_schema_permission"
  MOCronTrigger _ -> "cron_trigger"
  MOCustomTypes -> "custom_types"
  MOAction _ -> "action"
  MOActionPermission _ _ -> "action_permission"
  MOEndpoint _ -> "endpoint"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOSource name -> toTxt name
  MOSourceObjId source sourceObjId -> case sourceObjId of
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
      in tableObjectName <> " in " <> moiName (MOSourceObjId source $ SMOTable tableName)
  MORemoteSchema name -> toTxt name
  MORemoteSchemaPermissions name roleName ->
    toTxt roleName <> " permission in remote schema " <> toTxt name
  MOCronTrigger name -> toTxt name
  MOCustomTypes -> "custom_types"
  MOAction name -> toTxt name
  MOActionPermission name roleName -> toTxt roleName <> " permission in " <> toTxt name
  MOEndpoint name -> toTxt name

data MetadataObject
  = MetadataObject
  { _moId         :: !MetadataObjId
  , _moDefinition :: !Value
  } deriving (Eq)
$(makeLenses ''MetadataObject)

data InconsistentMetadata
  = InconsistentObject !Text !MetadataObject
  | ConflictingObjects !Text ![MetadataObject]
  | DuplicateObjects !MetadataObjId ![Value]
  deriving (Eq)
$(makePrisms ''InconsistentMetadata)

getInconsistentRemoteSchemas :: [InconsistentMetadata] -> [RemoteSchemaName]
getInconsistentRemoteSchemas =
  toListOf (traverse._InconsistentObject._2.moId._MORemoteSchema)

imObjectIds :: InconsistentMetadata -> [MetadataObjId]
imObjectIds = \case
  InconsistentObject _ metadata  -> [_moId metadata]
  ConflictingObjects _ metadatas -> map _moId metadatas
  DuplicateObjects objectId _    -> [objectId]

imReason :: InconsistentMetadata -> Text
imReason = \case
  InconsistentObject reason _ -> reason
  ConflictingObjects reason _ -> reason
  DuplicateObjects objectId _ -> "multiple definitions for " <> moiName objectId

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
        InconsistentObject _ metadata -> metadataObjectFields metadata
        ConflictingObjects _ metadatas ->
          [ "objects" .= map (object . metadataObjectFields) metadatas ]
        DuplicateObjects objectId definitions ->
          [ "type" .= String (moiTypeName objectId)
          , "definitions" .= definitions ]

      metadataObjectFields (MetadataObject objectId definition) =
        [ "type" .= String (moiTypeName objectId)
        , "definition" .= definition ]
