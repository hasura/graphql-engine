{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Metadata where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                  as AO
import qualified Data.HashMap.Strict.Extended        as M
import qualified Data.HashMap.Strict.InsOrd.Extended as OM
import qualified Data.HashSet                        as HS
import qualified Data.HashSet.InsOrd                 as HSIns
import qualified Data.List.Extended                  as L
import qualified Data.Text                           as T
import qualified Data.Text.Extended                  as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens                        hiding (set, (.=))
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.Incremental                  (Cacheable)
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.ApiLimit
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Endpoint
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.InheritedRoles
import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.RQL.Types.Metadata.Instances ()
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.SQL.Tag
import           Hasura.Session


-- | Raise exception if parsed list has multiple declarations
parseListAsMap
  :: (Hashable k, Eq k, Show k)
  => Text -> (a -> k) -> Parser [a] -> Parser (InsOrdHashMap k a)
parseListAsMap t mapFn listP = do
  list <- listP
  let duplicates = toList $ L.duplicates $ map mapFn list
  unless (null duplicates) $ fail $ T.unpack $
    "multiple declarations exist for the following " <> t <> " : "
    <> tshow duplicates
  pure $ oMapFromL mapFn list

data MetadataVersion
  = MVVersion1
  | MVVersion2
  | MVVersion3
  deriving (Show, Eq, Generic)

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

data ComputedFieldMetadata b
  = ComputedFieldMetadata
  { _cfmName       :: !ComputedFieldName
  , _cfmDefinition :: !(ComputedFieldDefinition b)
  , _cfmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance (Backend b) => Cacheable (ComputedFieldMetadata b)
instance (Backend b) => ToJSON (ComputedFieldMetadata b) where
  toJSON = genericToJSON hasuraJSON
instance (Backend b) => FromJSON (ComputedFieldMetadata b) where
  parseJSON = genericParseJSON hasuraJSON

data RemoteRelationshipMetadata
  = RemoteRelationshipMetadata
  { _rrmName       :: !RemoteRelationshipName
  , _rrmDefinition :: !RemoteRelationshipDef
  } deriving (Show, Eq, Generic)
instance Cacheable RemoteRelationshipMetadata
$(deriveJSON hasuraJSON ''RemoteRelationshipMetadata)
$(makeLenses ''RemoteRelationshipMetadata)

data RemoteSchemaPermissionMetadata
  = RemoteSchemaPermissionMetadata
  { _rspmRole       :: !RoleName
  , _rspmDefinition :: !RemoteSchemaPermissionDefinition
  , _rspmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance Cacheable RemoteSchemaPermissionMetadata
$(deriveJSON hasuraJSON{omitNothingFields=True} ''RemoteSchemaPermissionMetadata)
$(makeLenses ''RemoteSchemaPermissionMetadata)

data RemoteSchemaMetadata
  = RemoteSchemaMetadata
  { _rsmName        :: !RemoteSchemaName
  , _rsmDefinition  :: !RemoteSchemaDef
  , _rsmComment     :: !(Maybe Text)
  , _rsmPermissions :: ![RemoteSchemaPermissionMetadata]
  } deriving (Show, Eq, Generic)
instance Cacheable RemoteSchemaMetadata

instance FromJSON RemoteSchemaMetadata where
  parseJSON = withObject "RemoteSchemaMetadata" $ \obj ->
    RemoteSchemaMetadata
    <$> obj .: "name"
    <*> obj .: "definition"
    <*> obj .:? "comment"
    <*> obj .:? "permissions" .!= mempty
$(deriveToJSON hasuraJSON ''RemoteSchemaMetadata)
$(makeLenses ''RemoteSchemaMetadata)

type Relationships a = InsOrdHashMap RelName a
type ComputedFields b = InsOrdHashMap ComputedFieldName (ComputedFieldMetadata b)
type RemoteRelationships = InsOrdHashMap RemoteRelationshipName RemoteRelationshipMetadata
type Permissions a = InsOrdHashMap RoleName a
type EventTriggers = InsOrdHashMap TriggerName EventTriggerConf

data TableMetadata b
  = TableMetadata
  { _tmTable               :: !(TableName b)
  , _tmIsEnum              :: !Bool
  , _tmConfiguration       :: !(TableConfig b)
  , _tmObjectRelationships :: !(Relationships (ObjRelDef b))
  , _tmArrayRelationships  :: !(Relationships (ArrRelDef b))
  , _tmComputedFields      :: !(ComputedFields b)
  , _tmRemoteRelationships :: !RemoteRelationships
  , _tmInsertPermissions   :: !(Permissions (InsPermDef b))
  , _tmSelectPermissions   :: !(Permissions (SelPermDef b))
  , _tmUpdatePermissions   :: !(Permissions (UpdPermDef b))
  , _tmDeletePermissions   :: !(Permissions (DelPermDef b))
  , _tmEventTriggers       :: !EventTriggers
  } deriving (Generic)
deriving instance (Backend b) => Show (TableMetadata b)
deriving instance (Backend b) => Eq (TableMetadata b)
instance (Backend b) => Cacheable (TableMetadata b)
instance (Backend b) => ToJSON (TableMetadata b) where
  toJSON = genericToJSON hasuraJSON
$(makeLenses ''TableMetadata)

mkTableMeta :: TableName b -> Bool -> TableConfig b -> TableMetadata b
mkTableMeta qt isEnum config = TableMetadata qt isEnum config
  mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Backend b) => FromJSON (TableMetadata b) where
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

data FunctionMetadata b
  = FunctionMetadata
  { _fmFunction      :: !(FunctionName b)
  , _fmConfiguration :: !FunctionConfig
  , _fmPermissions   :: ![FunctionPermissionMetadata]
  } deriving (Generic)
deriving instance (Backend b) => Show (FunctionMetadata b)
deriving instance (Backend b) => Eq (FunctionMetadata b)
instance (Backend b) => Cacheable (FunctionMetadata b)
instance (Backend b) => ToJSON (FunctionMetadata b) where
  toJSON = genericToJSON hasuraJSON
$(makeLenses ''FunctionMetadata)

instance (Backend b) => FromJSON (FunctionMetadata b) where
  parseJSON = withObject "FunctionMetadata" $ \o ->
    FunctionMetadata
      <$> o .: "function"
      <*> o .:? "configuration" .!= emptyFunctionConfig
      <*> o .:? "permissions" .!= []

type Tables b = InsOrdHashMap (TableName b) (TableMetadata b)
type Functions b = InsOrdHashMap (FunctionName b) (FunctionMetadata b)
type RemoteSchemas = InsOrdHashMap RemoteSchemaName RemoteSchemaMetadata
type QueryCollections = InsOrdHashMap CollectionName CreateCollection
type Allowlist = HSIns.InsOrdHashSet CollectionReq
type Endpoints = InsOrdHashMap EndpointName CreateEndpoint
type Actions = InsOrdHashMap ActionName ActionMetadata
type CronTriggers = InsOrdHashMap TriggerName CronTriggerMetadata
type InheritedRoles = InsOrdHashMap RoleName AddInheritedRole

data SourceMetadata b
  = SourceMetadata
  { _smName          :: !SourceName
  , _smTables        :: !(Tables b)
  , _smFunctions     :: !(Functions b)
  , _smConfiguration :: !(SourceConnConfiguration b)
  } deriving (Generic)
$(makeLenses ''SourceMetadata)
deriving instance (Backend b) => Show (SourceMetadata b)
deriving instance (Backend b) => Eq (SourceMetadata b)
instance (Backend b) => Cacheable (SourceMetadata b)
instance (Backend b) => FromJSON (SourceMetadata b) where
  parseJSON = withObject "Object" $ \o -> do
    _smName          <- o .: "name"
    _smTables        <- oMapFromL _tmTable <$> o .: "tables"
    _smFunctions     <- oMapFromL _fmFunction <$> o .:? "functions" .!= []
    _smConfiguration <- o .: "configuration"
    pure SourceMetadata{..}

mkSourceMetadata
  :: forall (b :: BackendType)
   . BackendMetadata b
  => SourceName
  -> SourceConnConfiguration b
  -> BackendSourceMetadata
mkSourceMetadata name config =
  AB.mkAnyBackend $ SourceMetadata name mempty mempty config

type BackendSourceMetadata = AB.AnyBackend SourceMetadata

toSourceMetadata :: forall b. (BackendMetadata b) => Prism' BackendSourceMetadata (SourceMetadata b)
toSourceMetadata = prism' AB.mkAnyBackend AB.unpackAnyBackend

getSourceName :: BackendSourceMetadata -> SourceName
getSourceName e = AB.dispatchAnyBackend @BackendMetadata e _smName

type Sources = InsOrdHashMap SourceName BackendSourceMetadata

parseNonSourcesMetadata
  :: Object
  -> Parser
     ( RemoteSchemas
     , QueryCollections
     , Allowlist
     , CustomTypes
     , Actions
     , CronTriggers
     , ApiLimit
     , MetricsConfig
     , InheritedRoles
     )
parseNonSourcesMetadata o = do
  remoteSchemas <- parseListAsMap "remote schemas" _rsmName $
                   o .:? "remote_schemas" .!= []
  queryCollections <- parseListAsMap "query collections" _ccName $
                      o .:? "query_collections" .!= []
  allowlist <- o .:? "allowlist" .!= HSIns.empty
  customTypes <- o .:? "custom_types" .!= emptyCustomTypes
  actions <- parseListAsMap "actions" _amName $ o .:? "actions" .!= []
  cronTriggers <- parseListAsMap "cron triggers" ctName $
                  o .:? "cron_triggers" .!= []

  apiLimits     <- o .:? "api_limits" .!= emptyApiLimit
  metricsConfig <- o .:? "metrics_config" .!= emptyMetricsConfig
  inheritedRoles <- parseListAsMap "inherited roles" _adrRoleName $
                  o .:? "inherited_roles" .!= []

  pure ( remoteSchemas, queryCollections, allowlist, customTypes
       , actions, cronTriggers, apiLimits, metricsConfig, inheritedRoles
       )

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
  , _metaRestEndpoints    :: !Endpoints
  , _metaApiLimits        :: !ApiLimit
  , _metaMetricsConfig    :: !MetricsConfig
  , _metaInheritedRoles   :: !InheritedRoles
  } deriving (Show, Eq)
$(makeLenses ''Metadata)

instance FromJSON Metadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    when (version /= MVVersion3) $ fail $
      "unexpected metadata version from storage: " <> show version
    sources <- oMapFromL getSourceName <$> o .: "sources"
    endpoints <- oMapFromL _ceName <$> o .:? "rest_endpoints" .!= []
    (remoteSchemas, queryCollections, allowlist, customTypes,
     actions, cronTriggers, apiLimits, metricsConfig, inheritedRoles) <- parseNonSourcesMetadata o
    pure $ Metadata sources remoteSchemas queryCollections allowlist
           customTypes actions cronTriggers endpoints apiLimits metricsConfig inheritedRoles

emptyMetadata :: Metadata
emptyMetadata =
  Metadata mempty mempty mempty mempty emptyCustomTypes mempty mempty mempty
    emptyApiLimit emptyMetricsConfig mempty

tableMetadataSetter
  :: (BackendMetadata b)
  => SourceName -> TableName b -> ASetter' Metadata (TableMetadata b)
tableMetadataSetter source table =
  metaSources.ix source.toSourceMetadata.smTables.ix table

data MetadataNoSources
  = MetadataNoSources
  { _mnsTables           :: !(Tables 'Postgres)
  , _mnsFunctions        :: !(Functions 'Postgres)
  , _mnsRemoteSchemas    :: !RemoteSchemas
  , _mnsQueryCollections :: !QueryCollections
  , _mnsAllowlist        :: !Allowlist
  , _mnsCustomTypes      :: !CustomTypes
  , _mnsActions          :: !Actions
  , _mnsCronTriggers     :: !CronTriggers
  } deriving (Eq)
$(deriveToJSON hasuraJSON ''MetadataNoSources)

instance FromJSON MetadataNoSources where
  parseJSON = withObject "MetadataNoSources" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    (tables, functions) <-
      case version of
        MVVersion1 -> do
          tables       <- oMapFromL _tmTable <$> o .: "tables"
          functionList <- o .:? "functions" .!= []
          let functions = OM.fromList $ flip map functionList $
                \function -> (function, FunctionMetadata function emptyFunctionConfig mempty)
          pure (tables, functions)
        MVVersion2 -> do
          tables    <- oMapFromL _tmTable <$> o .: "tables"
          functions <- oMapFromL _fmFunction <$> o .:? "functions" .!= []
          pure (tables, functions)
        MVVersion3 -> fail "unexpected version for metadata without sources: 3"
    (remoteSchemas, queryCollections, allowlist, customTypes,
     actions, cronTriggers, _, _, _) <- parseNonSourcesMetadata o
    pure $ MetadataNoSources tables functions remoteSchemas queryCollections
                             allowlist customTypes actions cronTriggers

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
-- Rakesh says the consistency is really what's important here, rather than any particular
-- ordering (e.g. "version" being at the top).
--
-- Note: While modifying any part of the code below, make sure the encoded JSON of each type is
-- parsable via its 'FromJSON' instance.
--
-- TODO: we can use 'aeson-pretty' to serialize in a consistent way, and to specify a (partial)
-- order of keys, while getting the benefits of auto-generated To/FromJSON instances.
-- `FromJSON TableMetadata` complicates this though...
--
-- See: https://github.com/hasura/graphql-engine/issues/6348
metadataToOrdJSON :: Metadata -> AO.Value
metadataToOrdJSON ( Metadata
                    sources
                    remoteSchemas
                    queryCollections
                    allowlist
                    customTypes
                    actions
                    cronTriggers
                    endpoints
                    apiLimits
                    metricsConfig
                    inheritedRoles
                  ) = AO.object $ [ versionPair , sourcesPair] <>
                      catMaybes [ remoteSchemasPair
                                , queryCollectionsPair
                                , allowlistPair
                                , actionsPair
                                , customTypesPair
                                , cronTriggersPair
                                , endpointsPair
                                , apiLimitsPair
                                , metricsConfigPair
                                , inheritedRolesPair
                                ]
  where
    versionPair          = ("version", AO.toOrdered currentMetadataVersion)
    sourcesPair          = ("sources", AO.array $ map sourceMetaToOrdJSON $ sortOn getSourceName $ OM.elems sources
                           )
    remoteSchemasPair    = listToMaybeOrdPairSort "remote_schemas" remoteSchemaQToOrdJSON _rsmName remoteSchemas
    queryCollectionsPair = listToMaybeOrdPairSort "query_collections" createCollectionToOrdJSON _ccName queryCollections
    allowlistPair        = listToMaybeOrdPairSort "allowlist" AO.toOrdered _crCollection allowlist
    customTypesPair      = if customTypes == emptyCustomTypes then Nothing
                           else Just ("custom_types", customTypesToOrdJSON customTypes)
    actionsPair          = listToMaybeOrdPairSort "actions" actionMetadataToOrdJSON _amName actions
    cronTriggersPair     = listToMaybeOrdPairSort "cron_triggers" crontriggerQToOrdJSON ctName cronTriggers
    inheritedRolesPair   = listToMaybeOrdPairSort "inherited_roles" inheritedRolesQToOrdJSON _adrRoleName inheritedRoles
    endpointsPair        = listToMaybeOrdPairSort "rest_endpoints" AO.toOrdered _ceUrl endpoints

    apiLimitsPair        = if apiLimits == emptyApiLimit then Nothing
                           else Just ("api_limits", AO.toOrdered apiLimits)

    metricsConfigPair    = if metricsConfig == emptyMetricsConfig then Nothing
                           else Just ("metrics_config", AO.toOrdered metricsConfig)

    sourceMetaToOrdJSON :: BackendSourceMetadata -> AO.Value
    sourceMetaToOrdJSON exists =
      AB.dispatchAnyBackend @BackendMetadata exists $ \(SourceMetadata {..} :: SourceMetadata b) ->
        let sourceNamePair = ("name", AO.toOrdered _smName)
            sourceKind     = T.toTxt $ reify $ backendTag @b
            sourceKindPair = ("kind", AO.String sourceKind)
            tablesPair     = ("tables", AO.array $ map tableMetaToOrdJSON $ sortOn _tmTable $ OM.elems _smTables)
            functionsPair  = listToMaybeOrdPairSort "functions" functionMetadataToOrdJSON _fmFunction _smFunctions

            configurationPair = [("configuration", AO.toOrdered _smConfiguration)]

        in AO.object $ [sourceNamePair, sourceKindPair, tablesPair] <> maybeToList functionsPair <> configurationPair

    tableMetaToOrdJSON :: (Backend b) => TableMetadata b -> AO.Value
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
        objectRelationshipsPair = listToMaybeOrdPairSort "object_relationships"
                                  relDefToOrdJSON _rdName objectRelationships
        arrayRelationshipsPair = listToMaybeOrdPairSort "array_relationships"
                                 relDefToOrdJSON _rdName arrayRelationships
        computedFieldsPair = listToMaybeOrdPairSort "computed_fields"
                             computedFieldMetaToOrdJSON _cfmName computedFields
        remoteRelationshipsPair = listToMaybeOrdPairSort "remote_relationships"
                                  AO.toOrdered _rrmName remoteRelationships
        insertPermissionsPair = listToMaybeOrdPairSort "insert_permissions"
                                insPermDefToOrdJSON _pdRole insertPermissions
        selectPermissionsPair = listToMaybeOrdPairSort "select_permissions"
                                selPermDefToOrdJSON _pdRole selectPermissions
        updatePermissionsPair = listToMaybeOrdPairSort "update_permissions"
                                updPermDefToOrdJSON _pdRole updatePermissions
        deletePermissionsPair = listToMaybeOrdPairSort "delete_permissions"
                                delPermDefToOrdJSON _pdRole deletePermissions
        eventTriggersPair = listToMaybeOrdPairSort "event_triggers"
                            eventTriggerConfToOrdJSON etcName eventTriggers

        relDefToOrdJSON :: (ToJSON a) => RelDef a -> AO.Value
        relDefToOrdJSON (RelDef name using comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("using", AO.toOrdered using)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        computedFieldMetaToOrdJSON :: (Backend b) => ComputedFieldMetadata b -> AO.Value
        computedFieldMetaToOrdJSON (ComputedFieldMetadata name definition comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("definition", AO.toOrdered definition)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        insPermDefToOrdJSON :: forall b. (Backend b) => InsPermDef b -> AO.Value
        insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
          where
            insPermToOrdJSON (InsPerm check set columns mBackendOnly) =
              let columnsPair = ("columns",) . AO.toOrdered <$> columns
                  backendOnlyPair = ("backend_only",) . AO.toOrdered <$> mBackendOnly
              in AO.object $ [("check", AO.toOrdered check)]
                 <> catMaybes [maybeSetToMaybeOrdPair @b set, columnsPair, backendOnlyPair]

        selPermDefToOrdJSON :: Backend b => SelPermDef b -> AO.Value
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

        updPermDefToOrdJSON :: forall b. Backend b => UpdPermDef b -> AO.Value
        updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
          where
            updPermToOrdJSON (UpdPerm columns set fltr check) =
              AO.object $ [ ("columns", AO.toOrdered columns)
                          , ("filter", AO.toOrdered fltr)
                          , ("check", AO.toOrdered check)
                          ] <> catMaybes [maybeSetToMaybeOrdPair @b set]

        delPermDefToOrdJSON :: Backend b => DelPermDef b -> AO.Value
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

    functionMetadataToOrdJSON :: Backend b => FunctionMetadata b -> AO.Value
    functionMetadataToOrdJSON FunctionMetadata{..} =
      let confKeyPair =
            if _fmConfiguration == emptyFunctionConfig then []
            else pure ("configuration", AO.toOrdered _fmConfiguration)
          permissionsKeyPair =
            if null _fmPermissions then []
            else pure ("permissions", AO.toOrdered _fmPermissions)
      in AO.object $ [("function", AO.toOrdered _fmFunction)] <> confKeyPair <> permissionsKeyPair

    inheritedRolesQToOrdJSON :: AddInheritedRole -> AO.Value
    inheritedRolesQToOrdJSON AddInheritedRole{..} =
      AO.object [ ("role_name", AO.toOrdered _adrRoleName)
                , ("role_set", AO.toOrdered _adrRoleSet)
                ]

    remoteSchemaQToOrdJSON :: RemoteSchemaMetadata -> AO.Value
    remoteSchemaQToOrdJSON (RemoteSchemaMetadata name definition comment permissions) =
      AO.object $ [ ("name", AO.toOrdered name)
                  , ("definition", remoteSchemaDefToOrdJSON definition)
                  ]
                  <> catMaybes [ maybeCommentToMaybeOrdPair comment
                                , listToMaybeOrdPair
                                    "permissions"
                                    permsToMaybeOrdJSON
                                    permissions
                                ]
      where
        permsToMaybeOrdJSON :: RemoteSchemaPermissionMetadata -> AO.Value
        permsToMaybeOrdJSON (RemoteSchemaPermissionMetadata role defn permComment) =
          AO.object $ [("role", AO.toOrdered role)
                      ,("definition", AO.toOrdered defn)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

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

    -- Sort list before encoding to JSON value
    listToMaybeOrdPairSort
      :: (Foldable t, Ord b)
      => Text -> (a -> AO.Value) -> (a -> b) -> t a -> Maybe (Text, AO.Value)
    listToMaybeOrdPairSort name f sortF ta = case toList ta of
      []   -> Nothing
      list -> Just $ (name,) $ AO.array $ map f $ sortOn sortF list

    listToMaybeOrdPair
      :: (Foldable t)
      => Text -> (a -> AO.Value) -> t a -> Maybe (Text, AO.Value)
    listToMaybeOrdPair name f ta = case toList ta of
      []   -> Nothing
      list -> Just $ (name,) $ AO.array $ map f list

    maybeSetToMaybeOrdPair :: (Backend b) => Maybe (ColumnValues b Value) -> Maybe (Text, AO.Value)
    maybeSetToMaybeOrdPair set =
      set >>= \colVals -> if colVals == mempty then Nothing
                          else Just ("set", AO.toOrdered colVals)


    maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
    maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

    maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
    maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

    maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
    maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON

data CatalogStateType
  = CSTCli
  | CSTConsole
  deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = snakeCase . drop 3} ''CatalogStateType)

data SetCatalogState
  = SetCatalogState
  { _scsType  :: !CatalogStateType
  , _scsState :: !Value
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''SetCatalogState)

data CatalogState
  = CatalogState
  { _csId           :: !Text
  , _csCliState     :: !Value
  , _csConsoleState :: !Value
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''CatalogState)

data GetCatalogState
  = GetCatalogState
  deriving (Show, Eq)
$(deriveToJSON defaultOptions ''GetCatalogState)

instance FromJSON GetCatalogState where
  parseJSON _ = pure GetCatalogState
