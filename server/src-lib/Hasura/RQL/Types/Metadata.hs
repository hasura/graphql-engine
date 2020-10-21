{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Metadata where

import qualified Data.Aeson.Ordered                  as AO
import qualified Data.HashMap.Strict.Extended        as M
import qualified Data.HashSet                        as HS
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens                        hiding (set, (.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Language.Haskell.TH.Syntax          (Lift)


import           Hasura.Incremental                  (Cacheable)
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
import           Hasura.RQL.Types.Source
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

data SourceMetadataObjId
  = SMOTable !QualifiedTable
  | SMOFunction !QualifiedFunction
  | SMOTableObj !QualifiedTable !TableMetadataObjId
  deriving (Show, Eq, Generic)
instance Hashable SourceMetadataObjId

data MetadataObjId
  = MOSource !SourceName
  | MOSourceObjId !SourceName !SourceMetadataObjId
  | MORemoteSchema !RemoteSchemaName
  | MOCustomTypes
  | MOAction !ActionName
  | MOActionPermission !ActionName !RoleName
  | MOCronTrigger !TriggerName
  deriving (Show, Eq, Generic)
$(makePrisms ''MetadataObjId)
instance Hashable MetadataObjId

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOSource _ -> "source"
  MOSourceObjId _ sourceObjId -> case sourceObjId of
    SMOTable _  -> "table"
    SMOFunction _ -> "function"
    SMOTableObj _ tableObjectId -> case tableObjectId of
      MTORel _ relType        -> relTypeToTxt relType <> "_relation"
      MTOPerm _ permType      -> permTypeToCode permType <> "_permission"
      MTOTrigger _            -> "event_trigger"
      MTOComputedField _      -> "computed_field"
      MTORemoteRelationship _ -> "remote_relationship"
  MORemoteSchema _ -> "remote_schema"
  MOCronTrigger _ -> "cron_trigger"
  MOCustomTypes -> "custom_types"
  MOAction _ -> "action"
  MOActionPermission _ _ -> "action_permission"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOSource name -> dquoteTxt name
  MOSourceObjId source sourceObjId -> case sourceObjId of
    SMOTable name -> dquoteTxt name <> " in source " <> dquoteTxt source
    SMOFunction name -> dquoteTxt name <> " in source " <> dquoteTxt source
    SMOTableObj tableName tableObjectId ->
      let tableObjectName = case tableObjectId of
            MTORel name _              -> dquoteTxt name
            MTOComputedField name      -> dquoteTxt name
            MTORemoteRelationship name -> dquoteTxt name
            MTOPerm name _             -> dquoteTxt name
            MTOTrigger name            -> dquoteTxt name
      in tableObjectName <> " in " <> moiName (MOSourceObjId source $ SMOTable tableName)
  MORemoteSchema name -> dquoteTxt name
  MOCronTrigger name -> dquoteTxt name
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
  | MVVersion3
  deriving (Show, Eq, Lift, Generic)

instance ToJSON MetadataVersion where
  toJSON = \case
    MVVersion1 -> toJSON @Int 1
    MVVersion2 -> toJSON @Int 2
    MVVersion3 -> toJSON @Int 3

instance FromJSON MetadataVersion where
  parseJSON v = do
    version :: Int <- parseJSON v
    case version of
      1 -> pure MVVersion1
      2 -> pure MVVersion2
      3 -> pure MVVersion3
      i -> fail $ "expected 1, 2 or 3, encountered " ++ show i

currentMetadataVersion :: MetadataVersion
currentMetadataVersion = MVVersion3

data ComputedFieldMetadata
  = ComputedFieldMetadata
  { _cfmName       :: !ComputedFieldName
  , _cfmDefinition :: !ComputedFieldDefinition
  , _cfmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable ComputedFieldMetadata
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldMetadata)

data RemoteRelationshipMeta
  = RemoteRelationshipMeta
  { _rrmName       :: !RemoteRelationshipName
  , _rrmDefinition :: !RemoteRelationshipDef
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable RemoteRelationshipMeta
$(deriveJSON (aesonDrop 4 snakeCase) ''RemoteRelationshipMeta)
$(makeLenses ''RemoteRelationshipMeta)

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
     <*> (mapFromL _rdName   <$> o .:? orKey .!= [])
     <*> (mapFromL _rdName   <$> o .:? arKey .!= [])
     <*> (mapFromL _cfmName <$> o .:? cfKey .!= [])
     <*> (mapFromL _rrmName <$> o .:? rrKey .!= [])
     <*> (mapFromL _pdRole  <$> o .:? ipKey .!= [])
     <*> (mapFromL _pdRole  <$> o .:? spKey .!= [])
     <*> (mapFromL _pdRole  <$> o .:? upKey .!= [])
     <*> (mapFromL _pdRole  <$> o .:? dpKey .!= [])
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

type Tables = M.HashMap QualifiedTable TableMetadata
type Functions = M.HashMap QualifiedFunction FunctionMetadata
type RemoteSchemas = M.HashMap RemoteSchemaName AddRemoteSchemaQuery
type QueryCollections = M.HashMap CollectionName CreateCollection
type Allowlist = HS.HashSet CollectionReq
type Actions = M.HashMap ActionName ActionMetadata
type CronTriggers = M.HashMap TriggerName CronTriggerMetadata

data SourceConfiguration
  = SourceConfiguration
  { _scDatabaseUrl            :: !UrlConf
  , _scConnectionPoolSettings :: !SourceConnSettings
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable SourceConfiguration
$(deriveJSON (aesonDrop 3 snakeCase) ''SourceConfiguration)

-- data SourceConfiguration
--   = SCDefault -- ^ the default configuraion, to be resolved from --database-url option
--   | SCCustom !SourceCustomConfiguration -- ^ the custom configuration
--   deriving (Show, Eq, Lift, Generic)
-- instance Cacheable SourceConfiguration

data SourceMetadata
  = SourceMetadata
  { _smName          :: !SourceName
  , _smTables        :: !Tables
  , _smFunctions     :: !Functions
  , _smConfiguration :: !SourceConfiguration
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable SourceMetadata
$(makeLenses ''SourceMetadata)
instance FromJSON SourceMetadata where
  parseJSON = withObject "Object" $ \o -> do
    _smName          <- o .: "name"
    _smTables        <- mapFromL _tmTable <$> o .: "tables"
    _smFunctions     <- mapFromL _fmFunction <$> o .:? "functions" .!= []
    _smConfiguration <- o .: "configuration"
    pure SourceMetadata{..}

mkSourceMetadata
  :: SourceName -> UrlConf -> SourceConnSettings -> SourceMetadata
mkSourceMetadata name urlConf connSettings =
  SourceMetadata name mempty mempty $ SourceConfiguration urlConf connSettings

type Sources = M.HashMap SourceName SourceMetadata

-- | A complete GraphQL Engine metadata representation to be stored,
-- exported/replaced via metadata queries.
data Metadata
  = Metadata
  { _metaSources          :: !Sources
  , _metaRemoteSchemas    :: !RemoteSchemas
  , _metaQueryCollections :: !QueryCollections
  , _metaAllowlist        :: !Allowlist
  , _metaCustomTypes      :: !CustomTypes
  , _metaActions          :: !Actions
  , _metaCronTriggers     :: !CronTriggers
  } deriving (Show, Eq)
$(makeLenses ''Metadata)

parseNonSourcesMetadata
  :: Object
  -> Parser
     ( RemoteSchemas
     , QueryCollections
     , Allowlist
     , CustomTypes
     , Actions
     , CronTriggers
     )
parseNonSourcesMetadata o = do
  remoteSchemas <- (mapFromL _arsqName <$> o .:? "remote_schemas" .!= [])
  queryCollections <- (mapFromL _ccName <$> o .:? "query_collections" .!= [])
  allowlist <- o .:? "allowlist" .!= HS.empty
  customTypes <- o .:? "custom_types" .!= emptyCustomTypes
  actions <- (mapFromL _amName <$> o .:? "actions" .!= [])
  cronTriggers <- (mapFromL ctName <$> o .:? "cron_triggers" .!= [])
  pure ( remoteSchemas, queryCollections, allowlist, customTypes
       , actions, cronTriggers
       )

instance FromJSON Metadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    when (version /= MVVersion3) $ fail $
      "unexpected metadata version from storage: " <> show version
    sources <- mapFromL _smName <$> o .: "sources"
    (remoteSchemas, queryCollections, allowlist, customTypes,
     actions, cronTriggers) <- parseNonSourcesMetadata o
    pure $ Metadata sources remoteSchemas queryCollections allowlist
           customTypes actions cronTriggers

data MetadataNoSources
  = MetadataNoSources
  { _mnsTables           :: !Tables
  , _mnsFunctions        :: !Functions
  , _mnsRemoteSchemas    :: !RemoteSchemas
  , _mnsQueryCollections :: !QueryCollections
  , _mnsAllowlist        :: !Allowlist
  , _mnsCustomTypes      :: !CustomTypes
  , _mnsActions          :: !Actions
  , _mnsCronTriggers     :: !CronTriggers
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''MetadataNoSources)

instance FromJSON MetadataNoSources where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    (tables, functions) <-
      case version of
        MVVersion1 -> do
          tables       <- mapFromL _tmTable <$> o .: "tables"
          functionList <- o .:? "functions" .!= []
          let functions = M.fromList $ flip map functionList $
                \function -> (function, FunctionMetadata function emptyFunctionConfig)
          pure (tables, functions)
        MVVersion2 -> do
          tables    <- mapFromL _tmTable <$> o .: "tables"
          functions <- mapFromL _fmFunction <$> o .:? "functions" .!= []
          pure (tables, functions)
        MVVersion3 -> fail "unexpected version for metadata without sources: 3"
    (remoteSchemas, queryCollections, allowlist, customTypes,
     actions, cronTriggers) <- parseNonSourcesMetadata o
    pure $ MetadataNoSources tables functions remoteSchemas queryCollections
                             allowlist customTypes actions cronTriggers

emptyMetadata :: Metadata
emptyMetadata =
  Metadata mempty mempty mempty mempty emptyCustomTypes mempty mempty

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON

tableMetadataSetter
  :: SourceName -> QualifiedTable -> ASetter' Metadata TableMetadata
tableMetadataSetter source table =
  metaSources.ix source.smTables.ix table

-- | Encode 'Metadata' to JSON with deterministic ordering. Ordering of object keys and array
-- elements should  remain consistent across versions of graphql-engine if possible!
--
-- Note: While modifying any part of the code below, make sure the encoded JSON of each type is
-- parsable via its 'FromJSON' instance.
metadataToOrdJSON :: Metadata -> AO.Value
metadataToOrdJSON ( Metadata
                    sources
                    remoteSchemas
                    queryCollections
                    allowlist
                    customTypes
                    actions
                    cronTriggers
                  ) = AO.object $ [versionPair, sourcesPair] <>
                      catMaybes [ remoteSchemasPair
                                , queryCollectionsPair
                                , allowlistPair
                                , actionsPair
                                , customTypesPair
                                , cronTriggersPair
                                ]
  where
    versionPair = ("version", AO.toOrdered currentMetadataVersion)
    sourcesPair = ("sources", AO.array $ map sourceMetaToOrdJSON $ M.elems sources)

    remoteSchemasPair = listToMaybeOrdPair "remote_schemas" remoteSchemaQToOrdJSON $ M.elems remoteSchemas

    queryCollectionsPair = listToMaybeOrdPair "query_collections" createCollectionToOrdJSON $ M.elems queryCollections

    allowlistPair = listToMaybeOrdPair "allowlist" AO.toOrdered $ toList allowlist
    customTypesPair = if customTypes == emptyCustomTypes then Nothing
                      else Just ("custom_types", customTypesToOrdJSON customTypes)
    actionsPair = listToMaybeOrdPair "actions" actionMetadataToOrdJSON $ M.elems actions

    cronTriggersPair = listToMaybeOrdPair "cron_triggers" crontriggerQToOrdJSON $ M.elems cronTriggers

    sourceMetaToOrdJSON :: SourceMetadata -> AO.Value
    sourceMetaToOrdJSON SourceMetadata{..} =
      let sourceNamePair = ("name", AO.toOrdered _smName)
          tablesPair = ("tables", AO.array $ map tableMetaToOrdJSON $ M.elems _smTables)
          functionsPair = listToMaybeOrdPair "functions" functionMetadataToOrdJSON $ M.elems _smFunctions
          configurationPair = [("configuration", AO.toOrdered _smConfiguration)]
      in AO.object $ [sourceNamePair, tablesPair] <> maybeToList functionsPair <> configurationPair

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
                                  relDefToOrdJSON $ M.elems objectRelationships
        arrayRelationshipsPair = listToMaybeOrdPair "array_relationships"
                                 relDefToOrdJSON $ M.elems arrayRelationships
        computedFieldsPair = listToMaybeOrdPair "computed_fields"
                             computedFieldMetaToOrdJSON $ M.elems computedFields
        remoteRelationshipsPair = listToMaybeOrdPair "remote_relationships"
                                  AO.toOrdered $ M.elems remoteRelationships
        insertPermissionsPair = listToMaybeOrdPair "insert_permissions"
                                insPermDefToOrdJSON $ M.elems insertPermissions
        selectPermissionsPair = listToMaybeOrdPair "select_permissions"
                                selPermDefToOrdJSON $ M.elems selectPermissions
        updatePermissionsPair = listToMaybeOrdPair "update_permissions"
                                updPermDefToOrdJSON $ M.elems updatePermissions
        deletePermissionsPair = listToMaybeOrdPair "delete_permissions"
                                delPermDefToOrdJSON $ M.elems deletePermissions
        eventTriggersPair = listToMaybeOrdPair "event_triggers"
                            eventTriggerConfToOrdJSON $ M.elems eventTriggers

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

        insPermDefToOrdJSON :: InsPermDef -> AO.Value
        insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
          where
            insPermToOrdJSON (InsPerm check set columns mBackendOnly) =
              let columnsPair = ("columns",) . AO.toOrdered <$> columns
                  backendOnlyPair = ("backend_only",) . AO.toOrdered <$> mBackendOnly
              in AO.object $ [("check", AO.toOrdered check)]
                 <> catMaybes [maybeSetToMaybeOrdPair set, columnsPair, backendOnlyPair]

        selPermDefToOrdJSON :: SelPermDef -> AO.Value
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

        updPermDefToOrdJSON :: UpdPermDef -> AO.Value
        updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
          where
            updPermToOrdJSON (UpdPerm columns set fltr check) =
              AO.object $ [ ("columns", AO.toOrdered columns)
                          , ("filter", AO.toOrdered fltr)
                          , ("check", AO.toOrdered check)
                          ] <> catMaybes [maybeSetToMaybeOrdPair set]

        delPermDefToOrdJSON :: DelPermDef -> AO.Value
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
          | headerConfig == [] = Nothing
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
              <> catMaybes [ (("arguments", ) . AO.toOrdered) <$> argsValM
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
        actionDefinitionToOrdJSON (ActionDefinition args outputType actionType headers frwrdClientHdrs timeout handler) =
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
          <> (bool [("timeout",AO.toOrdered timeout)] mempty $ timeout == defaultActionTimeoutSecs)

        permToOrdJSON :: ActionPermissionMetadata -> AO.Value
        permToOrdJSON (ActionPermissionMetadata role permComment) =
          AO.object $ [("role", AO.toOrdered role)] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

    -- Utility functions
    listToMaybeOrdPair :: Text -> (a -> AO.Value) -> [a] -> Maybe (Text, AO.Value)
    listToMaybeOrdPair name f = \case
      []   -> Nothing
      list -> Just $ (name,) $ AO.array $ map f list

    maybeSetToMaybeOrdPair :: Maybe (ColumnValues Value) -> Maybe (Text, AO.Value)
    maybeSetToMaybeOrdPair set = set >>= \colVals -> if colVals == M.empty then Nothing
                                      else Just ("set", AO.toOrdered colVals)


    maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
    maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

    maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
    maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

    maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
    maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)

newtype MetadataModifier =
  MetadataModifier {unMetadataModifier :: Metadata -> Metadata}

instance Semigroup MetadataModifier where
  (MetadataModifier u1) <> (MetadataModifier u2)  = MetadataModifier $ u2 . u1

instance Monoid MetadataModifier where
  mempty = MetadataModifier id

noMetadataModify :: MetadataModifier
noMetadataModify = mempty

data CatalogStateType
  = CSTCli
  | CSTConsole
  deriving (Show, Eq, Lift)
$(deriveJSON defaultOptions{constructorTagModifier = snakeCase . drop 3} ''CatalogStateType)

data SetCatalogState
  = SetCatalogState
  { _scsType  :: !CatalogStateType
  , _scsState :: !Value
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''SetCatalogState)

data CatalogState
  = CatalogState
  { _csId           :: !Text
  , _csCliState     :: !Value
  , _csConsoleState :: !Value
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''CatalogState)

data GetCatalogState
  = GetCatalogState
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''GetCatalogState)

instance FromJSON GetCatalogState where
  parseJSON _ = pure GetCatalogState
