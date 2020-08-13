module Hasura.RQL.Types.Metadata where

import qualified Data.HashMap.Strict.Extended        as M
import qualified Data.HashSet                        as HS

import           Control.Lens                        hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax          (Lift)

import           Hasura.Prelude
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.Table
import           Hasura.Session
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
  | MOCronTrigger !TriggerName
  deriving (Show, Eq, Generic)
$(makePrisms ''MetadataObjId)
instance Hashable MetadataObjId

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOTable _ -> "table"
  MOFunction _ -> "function"
  MORemoteSchema _ -> "remote_schema"
  MOCronTrigger _ -> "cron_trigger"
  MOTableObj _ tableObjectId -> case tableObjectId of
    MTORel _ relType        -> relTypeToTxt relType <> "_relation"
    MTOPerm _ permType      -> permTypeToCode permType <> "_permission"
    MTOTrigger _            -> "event_trigger"
    MTOComputedField _      -> "computed_field"
    MTORemoteRelationship _ -> "remote_relationship"
  MOCustomTypes -> "custom_types"
  MOAction _ -> "action"
  MOActionPermission _ _ -> "action_permission"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOTable name -> dquoteTxt name
  MOFunction name -> dquoteTxt name
  MORemoteSchema name -> dquoteTxt name
  MOCronTrigger name -> dquoteTxt name
  MOTableObj tableName tableObjectId ->
    let tableObjectName = case tableObjectId of
          MTORel name _              -> dquoteTxt name
          MTOComputedField name      -> dquoteTxt name
          MTORemoteRelationship name -> dquoteTxt name
          MTOPerm name _             -> dquoteTxt name
          MTOTrigger name            -> dquoteTxt name
    in tableObjectName <> " in " <> moiName (MOTable tableName)
  MOCustomTypes -> "custom_types"
  MOAction name -> dquoteTxt name
  MOActionPermission name roleName -> dquoteTxt roleName <> " permission in " <> dquoteTxt name

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


data MetadataVersion
  = MVVersion1
  | MVVersion2
  deriving (Show, Eq, Lift, Generic)

instance ToJSON MetadataVersion where
  toJSON MVVersion1 = toJSON @Int 1
  toJSON MVVersion2 = toJSON @Int 2

instance FromJSON MetadataVersion where
  parseJSON v = do
    version :: Int <- parseJSON v
    case version of
      1 -> pure MVVersion1
      2 -> pure MVVersion2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

currentMetadataVersion :: MetadataVersion
currentMetadataVersion = MVVersion2

data ComputedFieldMetadata
  = ComputedFieldMetadata
  { _cfmName       :: !ComputedFieldName
  , _cfmDefinition :: !ComputedFieldDefinition
  , _cfmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldMetadata)

data RemoteRelationshipMeta
  = RemoteRelationshipMeta
  { _rrmName       :: !RemoteRelationshipName
  , _rrmDefinition :: !RemoteRelationshipDef
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''RemoteRelationshipMeta)

type Relationships a = M.HashMap RelName a
type ComputedFields = M.HashMap ComputedFieldName ComputedFieldMetadata
type RemoteRelationships = M.HashMap RemoteRelationshipName RemoteRelationshipMeta
type Permissions a = M.HashMap RoleName a
type EventTriggers = M.HashMap TriggerName EventTriggerConf

data TableMetadata
  = TableMetadata
  { _tmTable               :: !QualifiedTable
  , _tmIsEnum              :: !Bool
  , _tmConfiguration       :: !TableConfig
  , _tmObjectRelationships :: !(Relationships ObjRelDef)
  , _tmArrayRelationships  :: !(Relationships ArrRelDef)
  , _tmComputedFields      :: !ComputedFields
  , _tmRemoteRelationships :: !RemoteRelationships
  , _tmInsertPermissions   :: !(Permissions InsPermDef)
  , _tmSelectPermissions   :: !(Permissions SelPermDef)
  , _tmUpdatePermissions   :: !(Permissions UpdPermDef)
  , _tmDeletePermissions   :: !(Permissions DelPermDef)
  , _tmEventTriggers       :: !EventTriggers
  } deriving (Show, Eq, Lift, Generic)
$(makeLenses ''TableMetadata)

mkTableMeta :: QualifiedTable -> Bool -> TableConfig -> TableMetadata
mkTableMeta qt isEnum config = TableMetadata qt isEnum config
  mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance FromJSON TableMetadata where
  parseJSON = withObject "Object" $ \o -> do
    let unexpectedKeys = getUnexpectedKeys o
    unless (null unexpectedKeys) $
      fail $ "unexpected keys when parsing TableMetadata : "
      <> show (HS.toList unexpectedKeys)

    TableMetadata
     <$> o .: tableKey
     <*> o .:? isEnumKey .!= False
     <*> o .:? configKey .!= emptyTableConfig
     <*> (mapFromL rdName   <$> o .:? orKey .!= [])
     <*> (mapFromL rdName   <$> o .:? arKey .!= [])
     <*> (mapFromL _cfmName <$> o .:? cfKey .!= [])
     <*> (mapFromL _rrmName <$> o .:? rrKey .!= [])
     <*> (mapFromL pdRole   <$> o .:? ipKey .!= [])
     <*> (mapFromL pdRole   <$> o .:? spKey .!= [])
     <*> (mapFromL pdRole   <$> o .:? upKey .!= [])
     <*> (mapFromL pdRole   <$> o .:? dpKey .!= [])
     <*> (mapFromL etcName  <$> o .:? etKey .!= [])

    where
      tableKey = "table"
      isEnumKey = "is_enum"
      configKey = "configuration"
      orKey = "object_relationships"
      arKey = "array_relationships"
      ipKey = "insert_permissions"
      spKey = "select_permissions"
      upKey = "update_permissions"
      dpKey = "delete_permissions"
      etKey = "event_triggers"
      cfKey = "computed_fields"
      rrKey = "remote_relationships"

      getUnexpectedKeys o =
        HS.fromList (M.keys o) `HS.difference` expectedKeySet

      expectedKeySet =
        HS.fromList [ tableKey, isEnumKey, configKey, orKey
                    , arKey , ipKey, spKey, upKey, dpKey, etKey
                    , cfKey, rrKey
                    ]

type Tables = M.HashMap QualifiedTable TableMetadata
type Functions = M.HashMap QualifiedFunction TrackFunctionV2
type RemoteSchemas = M.HashMap RemoteSchemaName AddRemoteSchemaQuery
type QueryCollections = M.HashMap CollectionName CreateCollection
type Allowlist = HS.HashSet CollectionReq
type Actions = M.HashMap ActionName ActionMetadata
type CronTriggers = M.HashMap TriggerName CronTriggerMetadata

data FunctionsMetadata
  = FMVersion1 !(HS.HashSet QualifiedFunction)
  | FMVersion2 !Functions
  deriving (Show, Eq, Lift, Generic)

instance ToJSON FunctionsMetadata where
  toJSON (FMVersion1 qualifiedFunctions) = toJSON qualifiedFunctions
  toJSON (FMVersion2 functionsV2)        = toJSON functionsV2

-- | A complete GraphQL Engine metadata representation to be stored,
-- exported/replaced via metadata queries.
data Metadata
  = Metadata
  { _metaVersion          :: !MetadataVersion
  , _metaTables           :: !Tables
  , _metaFunctions        :: !FunctionsMetadata
  , _metaRemoteSchemas    :: !RemoteSchemas
  , _metaQueryCollections :: !QueryCollections
  , _metaAllowlist        :: !Allowlist
  , _metaCustomTypes      :: !CustomTypes
  , _metaActions          :: !Actions
  , _metaCronTriggers     :: !CronTriggers
  } deriving (Show, Eq)
$(makeLenses ''Metadata)

instance FromJSON Metadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    Metadata version
      <$> (mapFromL _tmTable <$> o .: "tables")
      <*> (o .:? "functions" >>= parseFunctions version)
      <*> (mapFromL _arsqName <$> o .:? "remote_schemas" .!= [])
      <*> (mapFromL _ccName <$> o .:? "query_collections" .!= [])
      <*> o .:? "allowlist" .!= HS.empty
      <*> o .:? "custom_types" .!= emptyCustomTypes
      <*> (mapFromL _amName <$> o .:? "actions" .!= [])
      <*> (mapFromL ctName <$> o .:? "cron_triggers" .!= [])
    where
      parseFunctions version maybeValue =
        case version of
          MVVersion1 -> FMVersion1 <$> maybe (pure mempty) parseJSON maybeValue
          MVVersion2 -> FMVersion2 <$> (mapFromL _tfv2Function <$> maybe (pure []) parseJSON maybeValue)
