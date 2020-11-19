{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Metadata where


import           Data.Aeson
import           Data.Text.Extended
import           Hasura.Prelude

import qualified Data.Aeson.Ordered                  as AO
import qualified Data.HashMap.Strict.Extended        as M
import qualified Data.HashMap.Strict.InsOrd.Extended as OM
import qualified Data.HashSet                        as HS
import qualified Data.HashSet.InsOrd                 as HSIns
import qualified Data.List.Extended                  as L
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens                        hiding (set, (.=))
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Language.Haskell.TH.Syntax          (Lift)


import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                  (Cacheable)
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
import           Hasura.SQL.Backend


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
  MOTable name -> toTxt name
  MOFunction name -> toTxt name
  MORemoteSchema name -> toTxt name
  MOCronTrigger name -> toTxt name
  MOTableObj tableName tableObjectId ->
    let tableObjectName = case tableObjectId of
          MTORel name _              -> toTxt name
          MTOComputedField name      -> toTxt name
          MTORemoteRelationship name -> toTxt name
          MTOPerm name _             -> toTxt name
          MTOTrigger name            -> toTxt name
    in tableObjectName <> " in " <> moiName (MOTable tableName)
  MOCustomTypes -> "custom_types"
  MOAction name -> toTxt name
  MOActionPermission name roleName -> toTxt roleName <> " permission in " <> toTxt name

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

-- | Raise exception if parsed list has multiple declarations
parseListAsMap
  :: (Hashable k, Eq k, Show k)
  => Text -> (a -> k) -> Parser [a] -> Parser (InsOrdHashMap k a)
parseListAsMap t mapFn listP = do
  list <- listP
  let duplicates = toList $ L.duplicates $ map mapFn list
  unless (null duplicates) $ fail $ T.unpack $
    "multiple declarations exist for the following " <> t <> " : "
    <> T.pack (show duplicates)
  pure $ oMapFromL mapFn list

data MetadataVersion
  = MVVersion1
  | MVVersion2
  deriving (Show, Eq, Lift, Generic)

instance ToJSON MetadataVersion where
  toJSON = \case
    MVVersion1 -> toJSON @Int 1
    MVVersion2 -> toJSON @Int 2

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
instance Cacheable ComputedFieldMetadata
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldMetadata)

data RemoteRelationshipMetadata
  = RemoteRelationshipMetadata
  { _rrmName       :: !RemoteRelationshipName
  , _rrmDefinition :: !RemoteRelationshipDef
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable RemoteRelationshipMetadata
$(deriveJSON (aesonDrop 4 snakeCase) ''RemoteRelationshipMetadata)
$(makeLenses ''RemoteRelationshipMetadata)

type Relationships a = InsOrdHashMap RelName a
type ComputedFields = InsOrdHashMap ComputedFieldName ComputedFieldMetadata
type RemoteRelationships = InsOrdHashMap RemoteRelationshipName RemoteRelationshipMetadata
type Permissions a = InsOrdHashMap RoleName a
type EventTriggers = InsOrdHashMap TriggerName EventTriggerConf

data TableMetadata
  = TableMetadata
  { _tmTable               :: !QualifiedTable
  , _tmIsEnum              :: !Bool
  , _tmConfiguration       :: !TableConfig
  , _tmObjectRelationships :: !(Relationships ObjRelDef)
  , _tmArrayRelationships  :: !(Relationships ArrRelDef)
  , _tmComputedFields      :: !ComputedFields
  , _tmRemoteRelationships :: !RemoteRelationships
  , _tmInsertPermissions   :: !(Permissions (InsPermDef 'Postgres))
  , _tmSelectPermissions   :: !(Permissions (SelPermDef 'Postgres))
  , _tmUpdatePermissions   :: !(Permissions (UpdPermDef 'Postgres))
  , _tmDeletePermissions   :: !(Permissions (DelPermDef 'Postgres))
  , _tmEventTriggers       :: !EventTriggers
  } deriving (Show, Eq, Generic)
instance Cacheable TableMetadata
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableMetadata)
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
     <*> parseListAsMap "object relationships" _rdName  (o .:? orKey .!= [])
     <*> parseListAsMap "array relationships" _rdName   (o .:? arKey .!= [])
     <*> parseListAsMap "computed fields" _cfmName      (o .:? cfKey .!= [])
     <*> parseListAsMap "remote relationships" _rrmName (o .:? rrKey .!= [])
     <*> parseListAsMap "insert permissions" _pdRole    (o .:? ipKey .!= [])
     <*> parseListAsMap "select permissions" _pdRole    (o .:? spKey .!= [])
     <*> parseListAsMap "update permissions" _pdRole    (o .:? upKey .!= [])
     <*> parseListAsMap "delete permissions" _pdRole    (o .:? dpKey .!= [])
     <*> parseListAsMap "event triggers" etcName        (o .:? etKey .!= [])

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

data FunctionMetadata
  = FunctionMetadata
  { _fmFunction      :: !QualifiedFunction
  , _fmConfiguration :: !FunctionConfig
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable FunctionMetadata
$(makeLenses ''FunctionMetadata)
$(deriveToJSON (aesonDrop 3 snakeCase) ''FunctionMetadata)

instance FromJSON FunctionMetadata where
  parseJSON = withObject "Object" $ \o ->
    FunctionMetadata
      <$> o .: "function"
      <*> o .:? "configuration" .!= emptyFunctionConfig

type Tables = InsOrdHashMap QualifiedTable TableMetadata
type Functions = InsOrdHashMap QualifiedFunction FunctionMetadata
type RemoteSchemas = InsOrdHashMap RemoteSchemaName AddRemoteSchemaQuery
type QueryCollections = InsOrdHashMap CollectionName CreateCollection
type Allowlist = HSIns.InsOrdHashSet CollectionReq
type Actions = InsOrdHashMap ActionName ActionMetadata
type CronTriggers = InsOrdHashMap TriggerName CronTriggerMetadata

parseNonPostgresMetadata
  :: Object
  -> Parser
     ( RemoteSchemas
     , QueryCollections
     , Allowlist
     , CustomTypes
     , Actions
     , CronTriggers
     )
parseNonPostgresMetadata o = do
  remoteSchemas <- parseListAsMap "remote schemas" _arsqName $
                   o .:? "remote_schemas" .!= []
  queryCollections <- parseListAsMap "query collections" _ccName $
                      o .:? "query_collections" .!= []
  allowlist <- o .:? "allowlist" .!= HSIns.empty
  customTypes <- o .:? "custom_types" .!= emptyCustomTypes
  actions <- parseListAsMap "actions" _amName $ o .:? "actions" .!= []
  cronTriggers <- parseListAsMap "cron triggers" ctName $
                  o .:? "cron_triggers" .!= []
  pure ( remoteSchemas, queryCollections, allowlist, customTypes
       , actions, cronTriggers
       )

-- | A complete GraphQL Engine metadata representation to be stored,
-- exported/replaced via metadata queries.
data Metadata
  = Metadata
  { _mnsTables           :: !Tables
  , _mnsFunctions        :: !Functions
  , _mnsRemoteSchemas    :: !RemoteSchemas
  , _mnsQueryCollections :: !QueryCollections
  , _mnsAllowlist        :: !Allowlist
  , _mnsCustomTypes      :: !CustomTypes
  , _mnsActions          :: !Actions
  , _mnsCronTriggers     :: !CronTriggers
  } deriving (Show, Eq)

instance FromJSON Metadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    tables  <- parseListAsMap "tables" _tmTable $ o .: "tables"
    functions <-
      case version of
        MVVersion1 -> do
          functions <- parseListAsMap "functions" id $ o .:? "functions" .!= []
          pure $ flip OM.map functions $
            \function -> FunctionMetadata function emptyFunctionConfig
        MVVersion2 -> parseListAsMap "functions" _fmFunction $ o .:? "functions" .!= []
    (remoteSchemas, queryCollections, allowlist, customTypes,
     actions, cronTriggers) <- parseNonPostgresMetadata o
    pure $ Metadata tables functions remoteSchemas queryCollections
                    allowlist customTypes actions cronTriggers

emptyMetadata :: Metadata
emptyMetadata =
  Metadata mempty mempty mempty mempty mempty emptyCustomTypes mempty mempty

newtype MetadataModifier =
  MetadataModifier {unMetadataModifier :: Metadata -> Metadata}

instance Semigroup MetadataModifier where
  (MetadataModifier u1) <> (MetadataModifier u2)  = MetadataModifier $ u2 . u1

instance Monoid MetadataModifier where
  mempty = MetadataModifier id

noMetadataModify :: MetadataModifier
noMetadataModify = mempty

-- | Encode 'Metadata' to JSON with deterministic ordering. Ordering of object keys and array
-- elements should  remain consistent across versions of graphql-engine if possible!
--
-- Note: While modifying any part of the code below, make sure the encoded JSON of each type is
-- parsable via its 'FromJSON' instance.
metadataToOrdJSON :: Metadata -> AO.Value
metadataToOrdJSON ( Metadata
                    tables
                    functions
                    remoteSchemas
                    queryCollections
                    allowlist
                    customTypes
                    actions
                    cronTriggers
                  ) = AO.object $ [versionPair, tablesPair] <>
                      catMaybes [ functionsPair
                                , remoteSchemasPair
                                , queryCollectionsPair
                                , allowlistPair
                                , actionsPair
                                , customTypesPair
                                , cronTriggersPair
                                ]
  where
    versionPair          = ("version", AO.toOrdered currentMetadataVersion)
    tablesPair           = ("tables", AO.array $ map tableMetaToOrdJSON $ OM.elems tables)
    functionsPair        = listToMaybeOrdPair "functions" functionMetadataToOrdJSON functions
    remoteSchemasPair    = listToMaybeOrdPair "remote_schemas" remoteSchemaQToOrdJSON remoteSchemas
    queryCollectionsPair = listToMaybeOrdPair "query_collections" createCollectionToOrdJSON queryCollections
    allowlistPair        = listToMaybeOrdPair "allowlist" AO.toOrdered allowlist
    customTypesPair      = if customTypes == emptyCustomTypes then Nothing
                           else Just ("custom_types", customTypesToOrdJSON customTypes)
    actionsPair          = listToMaybeOrdPair "actions" actionMetadataToOrdJSON actions
    cronTriggersPair     = listToMaybeOrdPair "cron_triggers" crontriggerQToOrdJSON cronTriggers

    tableMetaToOrdJSON :: TableMetadata -> AO.Value
    tableMetaToOrdJSON ( TableMetadata
                         table
                         isEnum
                         config
                         objectRelationships
                         arrayRelationships
                         computedFields
                         remoteRelationships
                         insertPermissions
                         selectPermissions
                         updatePermissions
                         deletePermissions
                         eventTriggers
                       ) = AO.object $ [("table", AO.toOrdered table)]
                           <> catMaybes [ isEnumPair
                                        , configPair
                                        , objectRelationshipsPair
                                        , arrayRelationshipsPair
                                        , computedFieldsPair
                                        , remoteRelationshipsPair
                                        , insertPermissionsPair
                                        , selectPermissionsPair
                                        , updatePermissionsPair
                                        , deletePermissionsPair
                                        , eventTriggersPair
                                        ]
      where
        isEnumPair = if isEnum then Just ("is_enum", AO.toOrdered isEnum) else Nothing
        configPair = if config == emptyTableConfig then Nothing
                     else Just ("configuration" , AO.toOrdered config)
        objectRelationshipsPair = listToMaybeOrdPair "object_relationships"
                                  relDefToOrdJSON objectRelationships
        arrayRelationshipsPair = listToMaybeOrdPair "array_relationships"
                                 relDefToOrdJSON arrayRelationships
        computedFieldsPair = listToMaybeOrdPair "computed_fields"
                             computedFieldMetaToOrdJSON computedFields
        remoteRelationshipsPair = listToMaybeOrdPair "remote_relationships"
                                  AO.toOrdered remoteRelationships
        insertPermissionsPair = listToMaybeOrdPair "insert_permissions"
                                insPermDefToOrdJSON insertPermissions
        selectPermissionsPair = listToMaybeOrdPair "select_permissions"
                                selPermDefToOrdJSON selectPermissions
        updatePermissionsPair = listToMaybeOrdPair "update_permissions"
                                updPermDefToOrdJSON updatePermissions
        deletePermissionsPair = listToMaybeOrdPair "delete_permissions"
                                delPermDefToOrdJSON deletePermissions
        eventTriggersPair = listToMaybeOrdPair "event_triggers"
                            eventTriggerConfToOrdJSON eventTriggers

        relDefToOrdJSON :: (ToJSON a) => RelDef a -> AO.Value
        relDefToOrdJSON (RelDef name using comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("using", AO.toOrdered using)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        computedFieldMetaToOrdJSON :: ComputedFieldMetadata -> AO.Value
        computedFieldMetaToOrdJSON (ComputedFieldMetadata name definition comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("definition", AO.toOrdered definition)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        insPermDefToOrdJSON :: InsPermDef 'Postgres -> AO.Value
        insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
          where
            insPermToOrdJSON (InsPerm check set columns mBackendOnly) =
              let columnsPair = ("columns",) . AO.toOrdered <$> columns
                  backendOnlyPair = ("backend_only",) . AO.toOrdered <$> mBackendOnly
              in AO.object $ [("check", AO.toOrdered check)]
                 <> catMaybes [maybeSetToMaybeOrdPair set, columnsPair, backendOnlyPair]

        selPermDefToOrdJSON :: SelPermDef 'Postgres -> AO.Value
        selPermDefToOrdJSON = permDefToOrdJSON selPermToOrdJSON
          where
            selPermToOrdJSON (SelPerm columns fltr limit allowAgg computedFieldsPerm) =
              AO.object $ catMaybes [ columnsPair
                                    , computedFieldsPermPair
                                    , filterPair
                                    , limitPair
                                    , allowAggPair
                                    ]
              where
                columnsPair = Just ("columns", AO.toOrdered columns)
                computedFieldsPermPair = listToMaybeOrdPair "computed_fields" AO.toOrdered computedFieldsPerm
                filterPair = Just ("filter", AO.toOrdered fltr)
                limitPair = maybeAnyToMaybeOrdPair "limit" AO.toOrdered limit
                allowAggPair = if allowAgg
                               then Just ("allow_aggregations", AO.toOrdered allowAgg)
                               else Nothing

        updPermDefToOrdJSON :: UpdPermDef 'Postgres -> AO.Value
        updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
          where
            updPermToOrdJSON (UpdPerm columns set fltr check) =
              AO.object $ [ ("columns", AO.toOrdered columns)
                          , ("filter", AO.toOrdered fltr)
                          , ("check", AO.toOrdered check)
                          ] <> catMaybes [maybeSetToMaybeOrdPair set]

        delPermDefToOrdJSON :: DelPermDef 'Postgres -> AO.Value
        delPermDefToOrdJSON = permDefToOrdJSON AO.toOrdered

        permDefToOrdJSON :: (a -> AO.Value) -> PermDef a -> AO.Value
        permDefToOrdJSON permToOrdJSON (PermDef role permission comment) =
          AO.object $ [ ("role", AO.toOrdered role)
                      , ("permission", permToOrdJSON permission)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        eventTriggerConfToOrdJSON :: EventTriggerConf -> AO.Value
        eventTriggerConfToOrdJSON (EventTriggerConf name definition webhook webhookFromEnv retryConf headers) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("definition", AO.toOrdered definition)
                      , ("retry_conf", AO.toOrdered retryConf)
                      ] <> catMaybes [ maybeAnyToMaybeOrdPair "webhook" AO.toOrdered webhook
                                     , maybeAnyToMaybeOrdPair "webhook_from_env" AO.toOrdered webhookFromEnv
                                     , headers >>= listToMaybeOrdPair "headers" AO.toOrdered
                                     ]

    functionMetadataToOrdJSON :: FunctionMetadata -> AO.Value
    functionMetadataToOrdJSON FunctionMetadata{..} =
      AO.object $ [("function", AO.toOrdered _fmFunction)]
      <> if _fmConfiguration == emptyFunctionConfig then []
         else pure ("configuration", AO.toOrdered _fmConfiguration)

    remoteSchemaQToOrdJSON :: AddRemoteSchemaQuery -> AO.Value
    remoteSchemaQToOrdJSON (AddRemoteSchemaQuery name definition comment) =
      AO.object $ [ ("name", AO.toOrdered name)
                  , ("definition", remoteSchemaDefToOrdJSON definition)
                  ] <> catMaybes [maybeCommentToMaybeOrdPair comment]
      where
        remoteSchemaDefToOrdJSON :: RemoteSchemaDef -> AO.Value
        remoteSchemaDefToOrdJSON (RemoteSchemaDef url urlFromEnv headers frwrdClientHdrs timeout) =
          AO.object $ catMaybes [ maybeToPair "url" url
                                , maybeToPair "url_from_env" urlFromEnv
                                , maybeToPair "timeout_seconds" timeout
                                , headers >>= listToMaybeOrdPair "headers" AO.toOrdered
                                ] <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
          where
            maybeToPair n = maybeAnyToMaybeOrdPair n AO.toOrdered

    createCollectionToOrdJSON :: CreateCollection -> AO.Value
    createCollectionToOrdJSON (CreateCollection name definition comment) =
      AO.object $ [ ("name", AO.toOrdered name)
                  , ("definition", AO.toOrdered definition)
                  ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

    crontriggerQToOrdJSON :: CronTriggerMetadata -> AO.Value
    crontriggerQToOrdJSON
      (CronTriggerMetadata name webhook schedule payload retryConf headers includeInMetadata comment) =
      AO.object $
            [ ("name", AO.toOrdered name)
            , ("webhook", AO.toOrdered webhook)
            , ("schedule", AO.toOrdered schedule)
            , ("include_in_metadata", AO.toOrdered includeInMetadata)
            ]
            <> catMaybes
            [ maybeAnyToMaybeOrdPair "payload" AO.toOrdered payload
            , maybeAnyToMaybeOrdPair "retry_conf" AO.toOrdered (maybeRetryConfiguration retryConf)
            , maybeAnyToMaybeOrdPair "headers" AO.toOrdered (maybeHeader headers)
            , maybeAnyToMaybeOrdPair "comment" AO.toOrdered comment]
      where
        maybeRetryConfiguration retryConfig
          | retryConfig == defaultSTRetryConf = Nothing
          | otherwise = Just retryConfig

        maybeHeader headerConfig
          | null headerConfig = Nothing
          | otherwise = Just headerConfig

    customTypesToOrdJSON :: CustomTypes -> AO.Value
    customTypesToOrdJSON (CustomTypes inpObjs objs scalars enums) =
      AO.object . catMaybes $ [ listToMaybeOrdPair "input_objects" inputObjectToOrdJSON =<< inpObjs
                              , listToMaybeOrdPair "objects" objectTypeToOrdJSON =<< objs
                              , listToMaybeOrdPair "scalars" scalarTypeToOrdJSON =<< scalars
                              , listToMaybeOrdPair "enums" enumTypeToOrdJSON =<< enums
                              ]
      where
        inputObjectToOrdJSON :: InputObjectTypeDefinition -> AO.Value
        inputObjectToOrdJSON (InputObjectTypeDefinition tyName descM fields) =
          AO.object $ [ ("name", AO.toOrdered tyName)
                      , ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
                      ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]
          where
            fieldDefinitionToOrdJSON :: InputObjectFieldDefinition -> AO.Value
            fieldDefinitionToOrdJSON (InputObjectFieldDefinition fieldName fieldDescM ty) =
              AO.object $ [ ("name", AO.toOrdered fieldName)
                          , ("type", AO.toOrdered ty)
                          ]
              <> catMaybes [maybeDescriptionToMaybeOrdPair fieldDescM]

        objectTypeToOrdJSON :: ObjectType -> AO.Value
        objectTypeToOrdJSON (ObjectTypeDefinition tyName descM fields rels) =
          AO.object $ [ ("name", AO.toOrdered tyName)
                      , ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
                      ]
          <> catMaybes [ maybeDescriptionToMaybeOrdPair descM
                       , maybeAnyToMaybeOrdPair "relationships" AO.toOrdered rels
                       ]
          where
            fieldDefinitionToOrdJSON :: ObjectFieldDefinition GraphQLType -> AO.Value
            fieldDefinitionToOrdJSON (ObjectFieldDefinition fieldName argsValM fieldDescM ty) =
              AO.object $ [ ("name", AO.toOrdered fieldName)
                          , ("type", AO.toOrdered ty)
                          ]
              <> catMaybes [ ("arguments", ) . AO.toOrdered <$> argsValM
                           , maybeDescriptionToMaybeOrdPair fieldDescM
                           ]

        scalarTypeToOrdJSON :: ScalarTypeDefinition -> AO.Value
        scalarTypeToOrdJSON (ScalarTypeDefinition tyName descM) =
          AO.object $ [("name", AO.toOrdered tyName)]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]

        enumTypeToOrdJSON :: EnumTypeDefinition -> AO.Value
        enumTypeToOrdJSON (EnumTypeDefinition tyName descM values) =
          AO.object $ [ ("name", AO.toOrdered tyName)
                      , ("values", AO.toOrdered values)
                      ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]


    actionMetadataToOrdJSON :: ActionMetadata -> AO.Value
    actionMetadataToOrdJSON (ActionMetadata name comment definition permissions) =
      AO.object $ [ ("name", AO.toOrdered name)
                  , ("definition", actionDefinitionToOrdJSON definition)
                  ]
      <> catMaybes [ maybeCommentToMaybeOrdPair comment
                   , listToMaybeOrdPair "permissions" permToOrdJSON permissions
                   ]
      where
        argDefinitionToOrdJSON :: ArgumentDefinition GraphQLType -> AO.Value
        argDefinitionToOrdJSON (ArgumentDefinition argName ty descM) =
          AO.object $  [ ("name", AO.toOrdered argName)
                       , ("type", AO.toOrdered ty)
                       ]
          <> catMaybes [maybeAnyToMaybeOrdPair "description" AO.toOrdered descM]

        actionDefinitionToOrdJSON :: ActionDefinitionInput -> AO.Value
        actionDefinitionToOrdJSON (ActionDefinition args outputType actionType
                                   headers frwrdClientHdrs timeout handler) =
          let typeAndKind = case actionType of
                ActionQuery -> [("type", AO.toOrdered ("query" :: String))]
                ActionMutation kind -> [ ("type", AO.toOrdered ("mutation" :: String))
                                       , ("kind", AO.toOrdered kind)]
          in
          AO.object $ [ ("handler", AO.toOrdered handler)
                      , ("output_type", AO.toOrdered outputType)
                      ]
          <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
          <> catMaybes [ listToMaybeOrdPair "headers" AO.toOrdered headers
                       , listToMaybeOrdPair "arguments" argDefinitionToOrdJSON args]
          <> typeAndKind
          <> bool [("timeout",AO.toOrdered timeout)] mempty (timeout == defaultActionTimeoutSecs)

        permToOrdJSON :: ActionPermissionMetadata -> AO.Value
        permToOrdJSON (ActionPermissionMetadata role permComment) =
          AO.object $ [("role", AO.toOrdered role)] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

    -- Utility functions
    listToMaybeOrdPair :: (Foldable t) => Text -> (a -> AO.Value) -> t a -> Maybe (Text, AO.Value)
    listToMaybeOrdPair name f ta = case toList ta of
      []   -> Nothing
      list -> Just $ (name,) $ AO.array $ map f list

    maybeSetToMaybeOrdPair :: Maybe (ColumnValues Value) -> Maybe (Text, AO.Value)
    maybeSetToMaybeOrdPair set = set >>= \colVals -> if colVals == mempty then Nothing
                                      else Just ("set", AO.toOrdered colVals)


    maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
    maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

    maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
    maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

    maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
    maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON
