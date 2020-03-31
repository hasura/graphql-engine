module Hasura.RQL.Types.Metadata where

import qualified Data.HashMap.Strict.Extended   as M

import           Control.Lens                   hiding ((.=))
import           Data.Aeson
import           Hasura.Prelude

import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types

data TableMetadataObjId
  = MTORel !RelName !RelType
  | MTOComputedField !ComputedFieldName
  | MTOPerm !RoleName !PermType
  | MTOTrigger !TriggerName
  | MTORemoteRelationship !RemoteRelationshipName
  deriving (Show, Eq, Generic)
instance Hashable TableMetadataObjId

data MetadataObjId
  = MOTable !QualifiedTable
  | MOFunction !QualifiedFunction
  | MORemoteSchema !RemoteSchemaName
  | MOTableObj !QualifiedTable !TableMetadataObjId
  | MOCustomTypes
  | MOAction !ActionName
  | MOActionPermission !ActionName !RoleName
  deriving (Show, Eq, Generic)
$(makePrisms ''MetadataObjId)
instance Hashable MetadataObjId

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOTable _ -> "table"
  MOFunction _ -> "function"
  MORemoteSchema _ -> "remote_schema"
  MOTableObj _ tableObjectId -> case tableObjectId of
    MTORel _ relType   -> relTypeToTxt relType <> "_relation"
    MTOPerm _ permType -> permTypeToCode permType <> "_permission"
    MTOTrigger _       -> "event_trigger"
    MTOComputedField _ -> "computed_field"
    MTORemoteRelationship _ -> "remote_relationship"
  MOCustomTypes -> "custom_types"
  MOAction _ -> "action"
  MOActionPermission _ _ -> "action_permission"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOTable name -> dquoteTxt name
  MOFunction name -> dquoteTxt name
  MORemoteSchema name -> dquoteTxt name
  MOTableObj tableName tableObjectId ->
    let tableObjectName = case tableObjectId of
          MTORel name _         -> dquoteTxt name
          MTOComputedField name -> dquoteTxt name
          MTORemoteRelationship name -> dquoteTxt name
          MTOPerm name _        -> dquoteTxt name
          MTOTrigger name       -> dquoteTxt name
    in tableObjectName <> " in " <> moiName (MOTable tableName)
  MOCustomTypes -> "custom_types"
  MOAction name -> dquoteTxt name
  MOActionPermission name role -> dquoteTxt role <> " permission in " <> dquoteTxt name

data MetadataObject
  = MetadataObject
  { _moId         :: !MetadataObjId
  , _moDefinition :: !Value
  } deriving (Show, Eq)
$(makeLenses ''MetadataObject)

data InconsistentMetadata
  = InconsistentObject !Text !MetadataObject
  | ConflictingObjects !Text ![MetadataObject]
  | DuplicateObjects !MetadataObjId ![Value]
  deriving (Show, Eq)
$(makePrisms ''InconsistentMetadata)

getInconsistentRemoteSchemas :: [InconsistentMetadata] -> [RemoteSchemaName]
getInconsistentRemoteSchemas =
  toListOf (traverse._InconsistentObject._2.moId._MORemoteSchema)

imObjectIds :: InconsistentMetadata -> [MetadataObjId]
imObjectIds = \case
  InconsistentObject _ metadata -> [_moId metadata]
  ConflictingObjects _ metadatas -> map _moId metadatas
  DuplicateObjects objectId _ -> [objectId]

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
