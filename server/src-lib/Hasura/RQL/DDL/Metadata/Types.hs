-- | Types related to metadata management API

{-# LANGUAGE TypeApplications #-}
module Hasura.RQL.DDL.Metadata.Types
  ( currentMetadataVersion
  , MetadataVersion(..)
  , TableMeta(..)
  , tmTable
  , tmIsEnum
  , tmConfiguration
  , tmObjectRelationships
  , tmArrayRelationships
  , tmComputedFields
  , tmInsertPermissions
  , tmSelectPermissions
  , tmUpdatePermissions
  , tmDeletePermissions
  , tmEventTriggers
  , mkTableMeta
  , ReplaceMetadata(..)
  , replaceMetadataToOrdJSON
  , ActionMetadata(..)
  , ActionPermissionMetadata(..)
  , ComputedFieldMeta(..)
  , FunctionsMetadata(..)
  , ExportMetadata(..)
  , ClearMetadata(..)
  , ReloadMetadata(..)
  , DumpInternalState(..)
  , GetInconsistentMetadata(..)
  , DropInconsistentMetadata(..)
  ) where

import           Hasura.Prelude

import           Control.Lens                   hiding (set, (.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax     (Lift)

import qualified Data.Aeson.Ordered             as AO
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Language.GraphQL.Draft.Syntax  as G

import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.RQL.DDL.ComputedField   as ComputedField
import qualified Hasura.RQL.DDL.Permission      as Permission
import qualified Hasura.RQL.DDL.QueryCollection as Collection
import qualified Hasura.RQL.DDL.Relationship    as Relationship
import qualified Hasura.RQL.DDL.Schema          as Schema

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

data ComputedFieldMeta
  = ComputedFieldMeta
  { _cfmName       :: !ComputedFieldName
  , _cfmDefinition :: !ComputedField.ComputedFieldDefinition
  , _cfmComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''ComputedFieldMeta)

data TableMeta
  = TableMeta
  { _tmTable               :: !QualifiedTable
  , _tmIsEnum              :: !Bool
  , _tmConfiguration       :: !TableConfig
  , _tmObjectRelationships :: ![Relationship.ObjRelDef]
  , _tmArrayRelationships  :: ![Relationship.ArrRelDef]
  , _tmComputedFields      :: ![ComputedFieldMeta]
  , _tmInsertPermissions   :: ![Permission.InsPermDef]
  , _tmSelectPermissions   :: ![Permission.SelPermDef]
  , _tmUpdatePermissions   :: ![Permission.UpdPermDef]
  , _tmDeletePermissions   :: ![Permission.DelPermDef]
  , _tmEventTriggers       :: ![EventTriggerConf]
  } deriving (Show, Eq, Lift, Generic)
$(makeLenses ''TableMeta)

mkTableMeta :: QualifiedTable -> Bool -> TableConfig -> TableMeta
mkTableMeta qt isEnum config =
  TableMeta qt isEnum config [] [] [] [] [] [] [] []

instance FromJSON TableMeta where
  parseJSON (Object o) = do
    unless (null unexpectedKeys) $
      fail $ "unexpected keys when parsing TableMetadata : "
      <> show (HS.toList unexpectedKeys)

    TableMeta
     <$> o .: tableKey
     <*> o .:? isEnumKey .!= False
     <*> o .:? configKey .!= emptyTableConfig
     <*> o .:? orKey .!= []
     <*> o .:? arKey .!= []
     <*> o .:? cfKey .!= []
     <*> o .:? ipKey .!= []
     <*> o .:? spKey .!= []
     <*> o .:? upKey .!= []
     <*> o .:? dpKey .!= []
     <*> o .:? etKey .!= []

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

      unexpectedKeys =
        HS.fromList (HM.keys o) `HS.difference` expectedKeySet

      expectedKeySet =
        HS.fromList [ tableKey, isEnumKey, configKey, orKey
                    , arKey , ipKey, spKey, upKey, dpKey, etKey
                    , cfKey
                    ]

  parseJSON _ =
    fail "expecting an Object for TableMetadata"

data FunctionsMetadata
  = FMVersion1 ![QualifiedFunction]
  | FMVersion2 ![Schema.TrackFunctionV2]
  deriving (Show, Eq, Lift, Generic)

instance ToJSON FunctionsMetadata where
  toJSON (FMVersion1 qualifiedFunctions) = toJSON qualifiedFunctions
  toJSON (FMVersion2 functionsV2)        = toJSON functionsV2

data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

data ReplaceMetadata
  = ReplaceMetadata
  { aqVersion           :: !MetadataVersion
  , aqTables            :: ![TableMeta]
  , aqFunctions         :: !FunctionsMetadata
  , aqRemoteSchemas     :: ![AddRemoteSchemaQuery]
  , aqQueryCollections  :: ![Collection.CreateCollection]
  , aqAllowlist         :: ![Collection.CollectionReq]
  , aqCustomTypes       :: !CustomTypes
  , aqActions           :: ![ActionMetadata]
  , aqScheduledTriggers :: ![CreateScheduledTrigger]
  } deriving (Show, Eq)

instance FromJSON ReplaceMetadata where
  parseJSON = withObject "Object" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    ReplaceMetadata version
      <$> o .: "tables"
      <*> (o .:? "functions" >>= parseFunctions version)
      <*> o .:? "remote_schemas" .!= []
      <*> o .:? "query_collections" .!= []
      <*> o .:? "allow_list" .!= []
      <*> o .:? "custom_types" .!= emptyCustomTypes
      <*> o .:? "actions" .!= []
      <*> o .:? "scheduled_triggers" .!= []
    where
      parseFunctions version maybeValue =
        case version of
          MVVersion1 -> FMVersion1 <$> maybe (pure []) parseJSON maybeValue
          MVVersion2 -> FMVersion2 <$> maybe (pure []) parseJSON maybeValue

data ExportMetadata
  = ExportMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''ExportMetadata)

instance FromJSON ExportMetadata where
  parseJSON _ = return ExportMetadata

newtype ReloadMetadata
  = ReloadMetadata
  { _rmReloadRemoteSchemas :: Bool}
  deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 3 snakeCase) ''ReloadMetadata)

instance FromJSON ReloadMetadata where
  parseJSON = \case
    Object o -> ReloadMetadata <$> o .:? "reload_remote_schemas" .!= False
    _        -> pure $ ReloadMetadata False

data DumpInternalState
  = DumpInternalState
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''DumpInternalState)

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''GetInconsistentMetadata)

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

data DropInconsistentMetadata
 = DropInconsistentMetadata
 deriving(Show, Eq, Lift)
$(deriveToJSON defaultOptions ''DropInconsistentMetadata)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

instance ToJSON ReplaceMetadata where
  toJSON = AO.fromOrdered . replaceMetadataToOrdJSON

-- | Encode 'ReplaceMetadata' to JSON with deterministic ordering. Ordering of object keys and array
-- elements should  remain consistent across versions of graphql-engine if possible!
--
-- Note: While modifying any part of the code below, make sure the encoded JSON of each type is
-- parsable via its 'FromJSON' instance.
replaceMetadataToOrdJSON :: ReplaceMetadata -> AO.Value
replaceMetadataToOrdJSON ( ReplaceMetadata
                               version
                               tables
                               functions
                               remoteSchemas
                               queryCollections
                               allowlist
                               customTypes
                               actions
                               scheduledTriggers
                             ) = AO.object $ [versionPair, tablesPair] <>
                                 catMaybes [ functionsPair
                                           , remoteSchemasPair
                                           , queryCollectionsPair
                                           , allowlistPair
                                           , actionsPair
                                           , customTypesPair
                                           , scheduledTriggersPair
                                           ]
  where
    versionPair = ("version", AO.toOrdered version)
    tablesPair = ("tables", AO.array $ map tableMetaToOrdJSON tables)
    functionsPair = ("functions",) <$> functionsMetadataToOrdJSON functions

    remoteSchemasPair = listToMaybeOrdPair "remote_schemas" remoteSchemaQToOrdJSON remoteSchemas

    queryCollectionsPair = listToMaybeOrdPair "query_collections" createCollectionToOrdJSON queryCollections

    allowlistPair = listToMaybeOrdPair "allowlist" AO.toOrdered allowlist
    customTypesPair = if customTypes == emptyCustomTypes then Nothing
                      else Just ("custom_types", customTypesToOrdJSON customTypes)
    actionsPair = listToMaybeOrdPair "actions" actionMetadataToOrdJSON actions

    scheduledTriggersPair = listToMaybeOrdPair "scheduled_triggers" scheduledTriggerQToOrdJSON scheduledTriggers

    tableMetaToOrdJSON :: TableMeta -> AO.Value
    tableMetaToOrdJSON ( TableMeta
                         table
                         isEnum
                         config
                         objectRelationships
                         arrayRelationships
                         computedFields
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

        relDefToOrdJSON :: (ToJSON a) => Relationship.RelDef a -> AO.Value
        relDefToOrdJSON (Relationship.RelDef name using comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("using", AO.toOrdered using)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        computedFieldMetaToOrdJSON :: ComputedFieldMeta -> AO.Value
        computedFieldMetaToOrdJSON (ComputedFieldMeta name definition comment) =
          AO.object $ [ ("name", AO.toOrdered name)
                      , ("definition", AO.toOrdered definition)
                      ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

        insPermDefToOrdJSON :: Permission.InsPermDef -> AO.Value
        insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
          where
            insPermToOrdJSON (Permission.InsPerm check set columns mBackendOnly) =
              let columnsPair = ("columns",) . AO.toOrdered <$> columns
                  backendOnlyPair = ("backend_only",) . AO.toOrdered <$> mBackendOnly
              in AO.object $ [("check", AO.toOrdered check)]
                 <> catMaybes [maybeSetToMaybeOrdPair set, columnsPair, backendOnlyPair]

        selPermDefToOrdJSON :: Permission.SelPermDef -> AO.Value
        selPermDefToOrdJSON = permDefToOrdJSON selPermToOrdJSON
          where
            selPermToOrdJSON (Permission.SelPerm columns fltr limit allowAgg computedFieldsPerm) =
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

        updPermDefToOrdJSON :: Permission.UpdPermDef -> AO.Value
        updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
          where
            updPermToOrdJSON (Permission.UpdPerm columns set fltr check) =
              AO.object $ [ ("columns", AO.toOrdered columns)
                          , ("filter", AO.toOrdered fltr)
                          , ("check", AO.toOrdered check)
                          ] <> catMaybes [maybeSetToMaybeOrdPair set]

        delPermDefToOrdJSON :: Permission.DelPermDef -> AO.Value
        delPermDefToOrdJSON = permDefToOrdJSON AO.toOrdered

        permDefToOrdJSON :: (a -> AO.Value) -> Permission.PermDef a -> AO.Value
        permDefToOrdJSON permToOrdJSON (Permission.PermDef role permission comment) =
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

    functionsMetadataToOrdJSON :: FunctionsMetadata -> Maybe AO.Value
    functionsMetadataToOrdJSON fm =
      let withList _ []   = Nothing
          withList f list = Just $ f list
          functionV2ToOrdJSON (Schema.TrackFunctionV2 function config) =
            AO.object $ [("function", AO.toOrdered function)]
                        <> if config == Schema.emptyFunctionConfig then []
                           else pure ("configuration", AO.toOrdered config)
      in case fm of
        FMVersion1 functionsV1 -> withList AO.toOrdered functionsV1
        FMVersion2 functionsV2 -> withList (AO.array . map functionV2ToOrdJSON) functionsV2

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

    createCollectionToOrdJSON :: Collection.CreateCollection -> AO.Value
    createCollectionToOrdJSON (Collection.CreateCollection name definition comment) =
      AO.object $ [ ("name", AO.toOrdered name)
                  , ("definition", AO.toOrdered definition)
                  ] <> catMaybes [maybeCommentToMaybeOrdPair comment]

    scheduledTriggerQToOrdJSON :: CreateScheduledTrigger -> AO.Value
    scheduledTriggerQToOrdJSON
      (CreateScheduledTrigger name webhook schedule payload retryConf headers includeInMetadata comment) =
      AO.object $
            [ ("name", AO.toOrdered name)
            , ("webhook", AO.toOrdered webhook)
            , ("schedule", AO.toOrdered schedule)
            ]
            <> catMaybes
            [ maybeAnyToMaybeOrdPair "payload" AO.toOrdered payload
            , maybeAnyToMaybeOrdPair "retry_conf" AO.toOrdered (maybeRetryConfiguration retryConf)
            , maybeAnyToMaybeOrdPair "headers" AO.toOrdered (maybeHeader headers)
            , maybeAnyToMaybeOrdPair "comment" AO.toOrdered comment]
            <> if includeInMetadata
               then [("include_in_metadata", AO.toOrdered includeInMetadata)]
               else []
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

        objectTypeToOrdJSON :: ObjectTypeDefinition -> AO.Value
        objectTypeToOrdJSON (ObjectTypeDefinition tyName descM fields rels) =
          AO.object $ [ ("name", AO.toOrdered tyName)
                      , ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
                      ]
          <> catMaybes [ maybeDescriptionToMaybeOrdPair descM
                       , listToMaybeOrdPair "relationships" AO.toOrdered =<< rels
                       ]
          where
            fieldDefinitionToOrdJSON :: ObjectFieldDefinition -> AO.Value
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
        argDefinitionToOrdJSON :: ArgumentDefinition -> AO.Value
        argDefinitionToOrdJSON (ArgumentDefinition argName ty descM) =
          AO.object $  [ ("name", AO.toOrdered argName)
                       , ("type", AO.toOrdered ty)
                       ]
          <> catMaybes [maybeAnyToMaybeOrdPair "description" AO.toOrdered descM]

        actionDefinitionToOrdJSON :: ActionDefinitionInput -> AO.Value
        actionDefinitionToOrdJSON (ActionDefinition args outputType actionType headers frwrdClientHdrs handler) =
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

        permToOrdJSON :: ActionPermissionMetadata -> AO.Value
        permToOrdJSON (ActionPermissionMetadata role permComment) =
          AO.object $ [("role", AO.toOrdered role)] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

    -- Utility functions
    listToMaybeOrdPair :: Text -> (a -> AO.Value) -> [a] -> Maybe (Text, AO.Value)
    listToMaybeOrdPair name f = \case
      []   -> Nothing
      list -> Just $ (name,) $ AO.array $ map f list

    maybeSetToMaybeOrdPair :: Maybe (ColumnValues Value) -> Maybe (Text, AO.Value)
    maybeSetToMaybeOrdPair set = set >>= \colVals -> if colVals == HM.empty then Nothing
                                      else Just ("set", AO.toOrdered colVals)


    maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
    maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

    maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
    maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

    maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
    maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)
