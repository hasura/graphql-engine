-- | Helpers used in the implementations of 'metadataToOrdJSON' and
-- 'metadataToDTO'
module Hasura.RQL.Types.Metadata.Serialization
  ( actionMetadataToOrdJSONList,
    allowlistToOrdJSONList,
    apiLimitsToOrdJSON,
    backendConfigsToOrdJSON,
    cronTriggersToOrdJSONList,
    customTypesToOrdJSON,
    endpointsToOrdJSONList,
    inheritedRolesToOrdJSONList,
    introspectionDisabledRolesToOrdJSON,
    metricsConfigToOrdJSON,
    networkConfigToOrdJSON,
    queryCollectionsToOrdJSONList,
    remoteSchemasToOrdJSONList,
    sourcesToOrdJSONList,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Ordered qualified as AO
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Text.Extended qualified as T
import Data.Vector qualified as Vector
import Hasura.Prelude
import Hasura.RQL.Types.Action
  ( ActionDefinition (..),
    ActionDefinitionInput,
    ActionMetadata (..),
    ActionPermissionMetadata (..),
    ActionType (..),
    ArgumentDefinition (..),
  )
import Hasura.RQL.Types.Allowlist (AllowlistEntry (..), MetadataAllowlist)
import Hasura.RQL.Types.ApiLimit (ApiLimit, emptyApiLimit)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Column (ColumnValues)
import Hasura.RQL.Types.Common (Comment, MetricsConfig, commentToMaybeText, defaultActionTimeoutSecs, emptyMetricsConfig)
import Hasura.RQL.Types.CustomTypes
  ( CustomTypes (..),
    EnumTypeDefinition (..),
    GraphQLType (..),
    InputObjectFieldDefinition (..),
    InputObjectTypeDefinition (..),
    ObjectFieldDefinition (..),
    ObjectTypeDefinition (..),
    ScalarTypeDefinition (..),
    emptyCustomTypes,
  )
import Hasura.RQL.Types.Endpoint (EndpointMetadata (..))
import Hasura.RQL.Types.EventTrigger (EventTriggerConf (..))
import Hasura.RQL.Types.Function (emptyFunctionConfig)
import Hasura.RQL.Types.GraphqlSchemaIntrospection (SetGraphqlIntrospectionOptions)
import Hasura.RQL.Types.Metadata.Common
  ( Actions,
    BackendConfigWrapper (..),
    BackendSourceMetadata,
    ComputedFieldMetadata (..),
    CronTriggers,
    Endpoints,
    FunctionMetadata (..),
    InheritedRoles,
    RemoteSchemaMetadata (..),
    RemoteSchemaPermissionMetadata (..),
    RemoteSchemas,
    SourceMetadata (..),
    Sources,
    TableMetadata (..),
    getSourceName,
  )
import Hasura.RQL.Types.Network (Network, emptyNetwork)
import Hasura.RQL.Types.Permission
  ( AllowedRootFields (..),
    DelPermDef,
    InsPerm (..),
    InsPermDef,
    PermDef (..),
    SelPerm (..),
    SelPermDef,
    UpdPerm (..),
    UpdPermDef,
    unPermDefPermission,
  )
import Hasura.RQL.Types.QueryCollection (CreateCollection (..), QueryCollections)
import Hasura.RQL.Types.Relationships.Local (RelDef (..))
import Hasura.RQL.Types.Relationships.Remote (RemoteRelationship (..))
import Hasura.RQL.Types.RemoteSchema (RemoteSchemaDef (..))
import Hasura.RQL.Types.Roles (InheritedRole, Role (..))
import Hasura.RQL.Types.ScheduledTrigger (CronTriggerMetadata (..), defaultSTRetryConf)
import Hasura.RQL.Types.SourceCustomization (emptySourceCustomization)
import Hasura.RQL.Types.Table (emptyTableConfig)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.SQL.Tag (HasTag (backendTag), reify)
import Language.GraphQL.Draft.Syntax qualified as G

sourcesToOrdJSONList :: Sources -> AO.Array
sourcesToOrdJSONList sources =
  Vector.fromList $
    map sourceMetaToOrdJSON $ sortOn getSourceName $ OM.elems sources
  where
    sourceMetaToOrdJSON :: BackendSourceMetadata -> AO.Value
    sourceMetaToOrdJSON exists =
      AB.dispatchAnyBackend @Backend exists $ \(SourceMetadata {..} :: SourceMetadata b) ->
        let sourceNamePair = ("name", AO.toOrdered _smName)
            sourceKindPair = ("kind", AO.toOrdered _smKind)
            tablesPair = ("tables", AO.array $ map tableMetaToOrdJSON $ sortOn _tmTable $ OM.elems _smTables)
            functionsPair = listToMaybeOrdPairSort "functions" functionMetadataToOrdJSON _fmFunction _smFunctions
            configurationPair = [("configuration", AO.toOrdered _smConfiguration)]
            queryTagsConfigPair = maybe [] (\queryTagsConfig -> [("query_tags", AO.toOrdered queryTagsConfig)]) _smQueryTags

            customizationPair =
              guard (_smCustomization /= emptySourceCustomization)
                *> [("customization", AO.toOrdered _smCustomization)]
         in AO.object $ [sourceNamePair, sourceKindPair, tablesPair] <> maybeToList functionsPair <> configurationPair <> queryTagsConfigPair <> customizationPair

    tableMetaToOrdJSON :: (Backend b) => TableMetadata b -> AO.Value
    tableMetaToOrdJSON
      ( TableMetadata
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
          enableApolloFed
        ) =
        AO.object $
          [("table", AO.toOrdered table)]
            <> catMaybes
              [ isEnumPair,
                configPair,
                objectRelationshipsPair,
                arrayRelationshipsPair,
                computedFieldsPair,
                remoteRelationshipsPair,
                insertPermissionsPair,
                selectPermissionsPair,
                updatePermissionsPair,
                deletePermissionsPair,
                eventTriggersPair,
                apolloFedConfigPair
              ]
        where
          isEnumPair = if isEnum then Just ("is_enum", AO.toOrdered isEnum) else Nothing
          apolloFedConfigPair = fmap (\afConfig -> ("apollo_federation_config", AO.toOrdered afConfig)) enableApolloFed
          configPair =
            if config == emptyTableConfig
              then Nothing
              else Just ("configuration", AO.toOrdered config)
          objectRelationshipsPair =
            listToMaybeOrdPairSort
              "object_relationships"
              relDefToOrdJSON
              _rdName
              objectRelationships
          arrayRelationshipsPair =
            listToMaybeOrdPairSort
              "array_relationships"
              relDefToOrdJSON
              _rdName
              arrayRelationships
          computedFieldsPair =
            listToMaybeOrdPairSort
              "computed_fields"
              computedFieldMetaToOrdJSON
              _cfmName
              computedFields
          remoteRelationshipsPair =
            listToMaybeOrdPairSort
              "remote_relationships"
              AO.toOrdered
              _rrName
              remoteRelationships
          insertPermissionsPair =
            listToMaybeOrdPairSort
              "insert_permissions"
              insPermDefToOrdJSON
              _pdRole
              insertPermissions
          selectPermissionsPair =
            listToMaybeOrdPairSort
              "select_permissions"
              selPermDefToOrdJSON
              _pdRole
              selectPermissions
          updatePermissionsPair =
            listToMaybeOrdPairSort
              "update_permissions"
              updPermDefToOrdJSON
              _pdRole
              updatePermissions
          deletePermissionsPair =
            listToMaybeOrdPairSort
              "delete_permissions"
              delPermDefToOrdJSON
              _pdRole
              deletePermissions
          eventTriggersPair =
            listToMaybeOrdPairSort
              "event_triggers"
              eventTriggerConfToOrdJSON
              etcName
              eventTriggers

          relDefToOrdJSON :: (ToJSON a) => RelDef a -> AO.Value
          relDefToOrdJSON (RelDef name using comment) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("using", AO.toOrdered using)
              ]
                <> catMaybes [maybeCommentToMaybeOrdPair comment]

          computedFieldMetaToOrdJSON :: (Backend b) => ComputedFieldMetadata b -> AO.Value
          computedFieldMetaToOrdJSON (ComputedFieldMetadata name definition comment) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("definition", AO.toOrdered definition)
              ]
                <> catMaybes [commentToMaybeOrdPair comment]

          insPermDefToOrdJSON :: forall b. (Backend b) => InsPermDef b -> AO.Value
          insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
            where
              insPermToOrdJSON (InsPerm check set columns backendOnly) =
                let columnsPair = ("columns",) . AO.toOrdered <$> columns
                    backendOnlyPair =
                      if backendOnly
                        then Just ("backend_only", AO.toOrdered backendOnly)
                        else Nothing
                 in AO.object $
                      [("check", AO.toOrdered check)]
                        <> catMaybes [maybeSetToMaybeOrdPair @b set, columnsPair, backendOnlyPair]

          selPermDefToOrdJSON :: Backend b => SelPermDef b -> AO.Value
          selPermDefToOrdJSON = permDefToOrdJSON selPermToOrdJSON
            where
              selPermToOrdJSON (SelPerm columns fltr limit allowAgg computedFieldsPerm allowedQueryRootFieldTypes allowedSubscriptionRootFieldTypes) =
                AO.object $
                  catMaybes
                    [ columnsPair,
                      computedFieldsPermPair,
                      filterPair,
                      limitPair,
                      allowAggPair,
                      allowedQueryRootFieldsPair,
                      allowedSubscriptionRootFieldsPair
                    ]
                where
                  columnsPair = Just ("columns", AO.toOrdered columns)
                  computedFieldsPermPair = listToMaybeOrdPair "computed_fields" AO.toOrdered computedFieldsPerm
                  filterPair = Just ("filter", AO.toOrdered fltr)
                  limitPair = maybeAnyToMaybeOrdPair "limit" AO.toOrdered limit
                  allowAggPair =
                    if allowAgg
                      then Just ("allow_aggregations", AO.toOrdered allowAgg)
                      else Nothing
                  allowedQueryRootFieldsPair =
                    case allowedQueryRootFieldTypes of
                      ARFAllowAllRootFields -> Nothing
                      ARFAllowConfiguredRootFields configuredRootFields ->
                        Just ("query_root_fields", AO.toOrdered configuredRootFields)
                  allowedSubscriptionRootFieldsPair =
                    case allowedSubscriptionRootFieldTypes of
                      ARFAllowAllRootFields -> Nothing
                      ARFAllowConfiguredRootFields configuredRootFields ->
                        Just ("subscription_root_fields", AO.toOrdered configuredRootFields)

          updPermDefToOrdJSON :: forall b. Backend b => UpdPermDef b -> AO.Value
          updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
            where
              updPermToOrdJSON (UpdPerm columns set fltr check backendOnly) =
                let backendOnlyPair =
                      if backendOnly
                        then Just ("backend_only", AO.toOrdered backendOnly)
                        else Nothing
                 in AO.object $
                      [ ("columns", AO.toOrdered columns),
                        ("filter", AO.toOrdered fltr),
                        ("check", AO.toOrdered check)
                      ]
                        <> catMaybes [maybeSetToMaybeOrdPair @b set, backendOnlyPair]

          delPermDefToOrdJSON :: Backend b => DelPermDef b -> AO.Value
          delPermDefToOrdJSON = permDefToOrdJSON AO.toOrdered

          permDefToOrdJSON :: (a b -> AO.Value) -> PermDef b a -> AO.Value
          permDefToOrdJSON permToOrdJSON (PermDef role permission comment) =
            AO.object $
              [ ("role", AO.toOrdered role),
                ("permission", permToOrdJSON (unPermDefPermission permission))
              ]
                <> catMaybes [maybeCommentToMaybeOrdPair comment]

          eventTriggerConfToOrdJSON :: Backend b => EventTriggerConf b -> AO.Value
          eventTriggerConfToOrdJSON (EventTriggerConf name definition webhook webhookFromEnv retryConf headers reqTransform respTransform) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("definition", AO.toOrdered definition),
                ("retry_conf", AO.toOrdered retryConf)
              ]
                <> catMaybes
                  [ maybeAnyToMaybeOrdPair "webhook" AO.toOrdered webhook,
                    maybeAnyToMaybeOrdPair "webhook_from_env" AO.toOrdered webhookFromEnv,
                    headers >>= listToMaybeOrdPair "headers" AO.toOrdered,
                    fmap (("request_transform",) . AO.toOrdered) reqTransform,
                    fmap (("response_transform",) . AO.toOrdered) respTransform
                  ]

    functionMetadataToOrdJSON :: Backend b => FunctionMetadata b -> AO.Value
    functionMetadataToOrdJSON FunctionMetadata {..} =
      let confKeyPair =
            if _fmConfiguration == emptyFunctionConfig
              then []
              else pure ("configuration", AO.toOrdered _fmConfiguration)
          permissionsKeyPair =
            if null _fmPermissions
              then []
              else pure ("permissions", AO.toOrdered _fmPermissions)
          commentKeyPair =
            if isNothing _fmComment
              then []
              else pure ("comment", AO.toOrdered _fmComment)
       in AO.object $ [("function", AO.toOrdered _fmFunction)] <> confKeyPair <> permissionsKeyPair <> commentKeyPair

remoteSchemasToOrdJSONList :: RemoteSchemas -> Maybe AO.Array
remoteSchemasToOrdJSONList = listToMaybeArraySort remoteSchemaQToOrdJSON _rsmName
  where
    remoteSchemaQToOrdJSON :: RemoteSchemaMetadata -> AO.Value
    remoteSchemaQToOrdJSON (RemoteSchemaMetadata name definition comment permissions relationships) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", remoteSchemaDefToOrdJSON definition)
        ]
          <> catMaybes
            [ maybeCommentToMaybeOrdPair comment,
              listToMaybeOrdPair
                "permissions"
                permsToMaybeOrdJSON
                permissions,
              listToMaybeOrdPair
                "remote_relationships"
                AO.toOrdered
                relationships
            ]
      where
        permsToMaybeOrdJSON :: RemoteSchemaPermissionMetadata -> AO.Value
        permsToMaybeOrdJSON (RemoteSchemaPermissionMetadata role defn permComment) =
          AO.object $
            [ ("role", AO.toOrdered role),
              ("definition", AO.toOrdered defn)
            ]
              <> catMaybes [maybeCommentToMaybeOrdPair permComment]

        remoteSchemaDefToOrdJSON :: RemoteSchemaDef -> AO.Value
        remoteSchemaDefToOrdJSON (RemoteSchemaDef url urlFromEnv headers frwrdClientHdrs timeout customization) =
          AO.object $
            catMaybes
              [ maybeToPair "url" url,
                maybeToPair "url_from_env" urlFromEnv,
                maybeToPair "timeout_seconds" timeout,
                maybeToPair "customization" customization,
                headers >>= listToMaybeOrdPair "headers" AO.toOrdered
              ]
              <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
          where
            maybeToPair n = maybeAnyToMaybeOrdPair n AO.toOrdered

backendConfigsToOrdJSON :: BackendMap BackendConfigWrapper -> Maybe AO.Value
backendConfigsToOrdJSON = ifNotEmpty (== mempty) configsToOrdJSON
  where
    configsToOrdJSON :: BackendMap BackendConfigWrapper -> AO.Value
    configsToOrdJSON backendConfigs' =
      AO.object . sortOn fst $ backendConfigToOrdJSON <$> BackendMap.elems backendConfigs'

    backendConfigToOrdJSON :: AB.AnyBackend BackendConfigWrapper -> (Text, AO.Value)
    backendConfigToOrdJSON backendConfig =
      AB.dispatchAnyBackend @Backend backendConfig $ \((BackendConfigWrapper backendConfig') :: BackendConfigWrapper b) ->
        let backendTypeStr = T.toTxt $ reify $ backendTag @b
            val = AO.toOrdered backendConfig'
         in (backendTypeStr, val)

inheritedRolesToOrdJSONList :: InheritedRoles -> Maybe AO.Array
inheritedRolesToOrdJSONList = listToMaybeArraySort inheritedRolesQToOrdJSON _rRoleName
  where
    inheritedRolesQToOrdJSON :: InheritedRole -> AO.Value
    inheritedRolesQToOrdJSON (Role roleName roleSet) =
      AO.object
        [ ("role_name", AO.toOrdered roleName),
          ("role_set", AO.toOrdered roleSet)
        ]

queryCollectionsToOrdJSONList :: QueryCollections -> Maybe AO.Array
queryCollectionsToOrdJSONList = listToMaybeArraySort createCollectionToOrdJSON _ccName
  where
    createCollectionToOrdJSON :: CreateCollection -> AO.Value
    createCollectionToOrdJSON (CreateCollection name definition comment) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", AO.toOrdered definition)
        ]
          <> catMaybes [maybeCommentToMaybeOrdPair comment]

allowlistToOrdJSONList :: MetadataAllowlist -> Maybe AO.Array
allowlistToOrdJSONList = listToMaybeArraySort (AO.toOrdered . toJSON @AllowlistEntry) aeCollection

apiLimitsToOrdJSON :: ApiLimit -> Maybe AO.Value
apiLimitsToOrdJSON apiLimits
  | apiLimits == emptyApiLimit = Nothing
  | otherwise = Just $ AO.toOrdered apiLimits

cronTriggersToOrdJSONList :: CronTriggers -> Maybe AO.Array
cronTriggersToOrdJSONList = listToMaybeArraySort crontriggerQToOrdJSON ctName
  where
    crontriggerQToOrdJSON :: CronTriggerMetadata -> AO.Value
    crontriggerQToOrdJSON
      (CronTriggerMetadata name webhook schedule payload retryConf headers includeInMetadata comment reqTransform respTransform) =
        AO.object $
          [ ("name", AO.toOrdered name),
            ("webhook", AO.toOrdered webhook),
            ("schedule", AO.toOrdered schedule),
            ("include_in_metadata", AO.toOrdered includeInMetadata)
          ]
            <> catMaybes
              [ maybeAnyToMaybeOrdPair "payload" AO.toOrdered payload,
                maybeAnyToMaybeOrdPair "retry_conf" AO.toOrdered (maybeRetryConfiguration retryConf),
                maybeAnyToMaybeOrdPair "headers" AO.toOrdered (maybeHeader headers),
                maybeAnyToMaybeOrdPair "comment" AO.toOrdered comment,
                fmap (("request_transform",) . AO.toOrdered) reqTransform,
                fmap (("response_transform",) . AO.toOrdered) respTransform
              ]
        where
          maybeRetryConfiguration retryConfig
            | retryConfig == defaultSTRetryConf = Nothing
            | otherwise = Just retryConfig

          maybeHeader headerConfig
            | null headerConfig = Nothing
            | otherwise = Just headerConfig

customTypesToOrdJSON :: CustomTypes -> Maybe AO.Object
customTypesToOrdJSON customTypes@(CustomTypes inpObjs objs scalars enums)
  | customTypes == emptyCustomTypes = Nothing
  | otherwise =
    Just . AO.fromList . catMaybes $
      [ listToMaybeOrdPair "input_objects" inputObjectToOrdJSON inpObjs,
        listToMaybeOrdPair "objects" objectTypeToOrdJSON objs,
        listToMaybeOrdPair "scalars" scalarTypeToOrdJSON scalars,
        listToMaybeOrdPair "enums" enumTypeToOrdJSON enums
      ]
  where
    inputObjectToOrdJSON :: InputObjectTypeDefinition -> AO.Value
    inputObjectToOrdJSON (InputObjectTypeDefinition tyName descM fields) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
        ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]
      where
        fieldDefinitionToOrdJSON :: InputObjectFieldDefinition -> AO.Value
        fieldDefinitionToOrdJSON (InputObjectFieldDefinition fieldName fieldDescM ty) =
          AO.object $
            [ ("name", AO.toOrdered fieldName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes [maybeDescriptionToMaybeOrdPair fieldDescM]

    objectTypeToOrdJSON :: ObjectTypeDefinition -> AO.Value
    objectTypeToOrdJSON (ObjectTypeDefinition tyName descM fields rels) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
        ]
          <> catMaybes
            [ maybeDescriptionToMaybeOrdPair descM,
              listToMaybeOrdPair "relationships" AO.toOrdered rels
            ]
      where
        fieldDefinitionToOrdJSON :: ObjectFieldDefinition GraphQLType -> AO.Value
        fieldDefinitionToOrdJSON (ObjectFieldDefinition fieldName argsValM fieldDescM ty) =
          AO.object $
            [ ("name", AO.toOrdered fieldName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes
                [ ("arguments",) . AO.toOrdered <$> argsValM,
                  maybeDescriptionToMaybeOrdPair fieldDescM
                ]

    scalarTypeToOrdJSON :: ScalarTypeDefinition -> AO.Value
    scalarTypeToOrdJSON (ScalarTypeDefinition tyName descM) =
      AO.object $
        [("name", AO.toOrdered tyName)]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]

    enumTypeToOrdJSON :: EnumTypeDefinition -> AO.Value
    enumTypeToOrdJSON (EnumTypeDefinition tyName descM values) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("values", AO.toOrdered values)
        ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]

endpointsToOrdJSONList :: Endpoints -> Maybe AO.Array
endpointsToOrdJSONList = listToMaybeArraySort AO.toOrdered _ceUrl

introspectionDisabledRolesToOrdJSON :: SetGraphqlIntrospectionOptions -> Maybe AO.Value
introspectionDisabledRolesToOrdJSON = ifNotEmpty (== mempty) AO.toOrdered

metricsConfigToOrdJSON :: MetricsConfig -> Maybe AO.Value
metricsConfigToOrdJSON = ifNotEmpty (== emptyMetricsConfig) AO.toOrdered

networkConfigToOrdJSON :: Network -> Maybe AO.Value
networkConfigToOrdJSON = ifNotEmpty (== emptyNetwork) AO.toOrdered

actionMetadataToOrdJSONList :: Actions -> Maybe AO.Array
actionMetadataToOrdJSONList = listToMaybeArraySort actionMetadataToOrdJSON _amName
  where
    actionMetadataToOrdJSON :: ActionMetadata -> AO.Value
    actionMetadataToOrdJSON (ActionMetadata name comment definition permissions) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", actionDefinitionToOrdJSON definition)
        ]
          <> catMaybes
            [ maybeCommentToMaybeOrdPair comment,
              listToMaybeOrdPair "permissions" permToOrdJSON permissions
            ]
      where
        argDefinitionToOrdJSON :: ArgumentDefinition GraphQLType -> AO.Value
        argDefinitionToOrdJSON (ArgumentDefinition argName ty descM) =
          AO.object $
            [ ("name", AO.toOrdered argName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes [maybeAnyToMaybeOrdPair "description" AO.toOrdered descM]

        actionDefinitionToOrdJSON :: ActionDefinitionInput -> AO.Value
        actionDefinitionToOrdJSON
          ( ActionDefinition
              args
              outputType
              actionType
              headers
              frwrdClientHdrs
              timeout
              handler
              requestTransform
              responseTransform
            ) =
            let typeAndKind = case actionType of
                  ActionQuery -> [("type", AO.toOrdered ("query" :: String))]
                  ActionMutation kind ->
                    [ ("type", AO.toOrdered ("mutation" :: String)),
                      ("kind", AO.toOrdered kind)
                    ]
             in AO.object $
                  [ ("handler", AO.toOrdered handler),
                    ("output_type", AO.toOrdered outputType)
                  ]
                    <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
                    <> catMaybes
                      [ listToMaybeOrdPair "headers" AO.toOrdered headers,
                        listToMaybeOrdPair "arguments" argDefinitionToOrdJSON args,
                        fmap (("request_transform",) . AO.toOrdered) requestTransform,
                        fmap (("response_transform",) . AO.toOrdered) responseTransform
                      ]
                    <> typeAndKind
                    <> bool [("timeout", AO.toOrdered timeout)] mempty (timeout == defaultActionTimeoutSecs)

        permToOrdJSON :: ActionPermissionMetadata -> AO.Value
        permToOrdJSON (ActionPermissionMetadata role permComment) =
          AO.object $ [("role", AO.toOrdered role)] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

ifNotEmpty :: (a -> Bool) -> (a -> b) -> a -> Maybe b
ifNotEmpty isEmpty f x
  | isEmpty x = Nothing
  | otherwise = Just $ f x

-- | Sort list before encoding to JSON value
listToMaybeOrdPairSort ::
  (Foldable t, Ord b) =>
  Text ->
  (a -> AO.Value) ->
  (a -> b) ->
  t a ->
  Maybe (Text, AO.Value)
listToMaybeOrdPairSort name f sortF ta = case toList ta of
  [] -> Nothing
  list -> Just $ (name,) $ AO.array $ map f $ sortOn sortF list

-- | Sort list before encoding to JSON array (not value)
listToMaybeArraySort ::
  (Foldable t, Ord b) =>
  (a -> AO.Value) ->
  (a -> b) ->
  t a ->
  Maybe AO.Array
listToMaybeArraySort f sortF ta = case toList ta of
  [] -> Nothing
  list -> Just $ Vector.fromList $ map f $ sortOn sortF list

listToMaybeOrdPair ::
  (Foldable t) =>
  Text ->
  (a -> AO.Value) ->
  t a ->
  Maybe (Text, AO.Value)
listToMaybeOrdPair name f ta = case toList ta of
  [] -> Nothing
  list -> Just $ (name,) $ AO.array $ map f list

maybeSetToMaybeOrdPair :: (Backend b) => Maybe (ColumnValues b JSON.Value) -> Maybe (Text, AO.Value)
maybeSetToMaybeOrdPair set =
  set >>= \colVals ->
    if colVals == mempty
      then Nothing
      else Just ("set", AO.toOrdered colVals)

maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)

commentToMaybeOrdPair :: Comment -> Maybe (Text, AO.Value)
commentToMaybeOrdPair comment = (\val -> ("comment", AO.toOrdered val)) <$> commentToMaybeText comment
