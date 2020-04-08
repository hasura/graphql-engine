module Hasura.GraphQL.Schema.Action
  ( mkActionsSchema
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Coerce                   (coerce)

import           Hasura.GraphQL.Schema.Builder
import           Hasura.GraphQL.Schema.Common  (mkDescriptionWith)

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkAsyncActionSelectionType :: ActionName -> G.NamedType
mkAsyncActionSelectionType = G.NamedType . unActionName

mkAsyncActionQueryResponseObj
  :: ActionName
  -- Name of the action
  -> GraphQLType
  -- output type
  -> ObjTyInfo
mkAsyncActionQueryResponseObj actionName outputType =
  mkHsraObjTyInfo
  (Just description)
  (mkAsyncActionSelectionType actionName) -- "(action_name)"
  mempty -- no arguments
  (mapFromL _fiName fieldDefinitions)
  where
    description = G.Description $ "fields of action: " <>> actionName

    mkFieldDefinition (fieldName, fieldDescription, fieldType) =
      mkHsraObjFldInfo
      (Just fieldDescription)
      fieldName
      mempty
      fieldType

    fieldDefinitions = map mkFieldDefinition
      [ ( "id", "the unique id of an action"
        , G.toGT $ mkScalarTy PGUUID)
      , ( "created_at", "the time at which this action was created"
        , G.toGT $ mkScalarTy PGTimeStampTZ)
      , ( "errors", "errors related to the invocation"
        , G.toGT $ mkScalarTy PGJSON)
      , ( "output", "the output fields of this action"
        , unGraphQLType outputType)
      ]

mkQueryActionField
  :: ActionName
  -> ActionInfo
  -> [(PGCol, PGScalarType)]
  -> (ActionExecutionContext, ObjFldInfo)
mkQueryActionField actionName actionInfo definitionList =
  ( actionExecutionContext
  , fieldInfo
  )
  where
    definition = _aiDefinition actionInfo
    actionExecutionContext =
      ActionExecutionContext
      actionName
      (_adOutputType definition)
      (_aiOutputFields actionInfo)
      definitionList
      (_adHandler definition)
      (_adHeaders definition)
      (_adForwardClientHeaders definition)

    description = mkDescriptionWith (PGDescription <$> (_aiComment actionInfo)) $
                  "perform the action: " <>> actionName

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName $ map mkActionArgument $ _adArguments definition)
      actionFieldResponseType

    mkActionArgument argument =
      InpValInfo (_argDescription argument) (unArgumentName $ _argName argument)
      Nothing $ unGraphQLType $ _argType argument

    actionFieldResponseType = unGraphQLType $ _adOutputType definition

mkMutationActionField
  :: ActionName
  -> ActionInfo
  -> [(PGCol, PGScalarType)]
  -> ActionMutationKind
  -> (ActionMutationExecutionContext, ObjFldInfo)
mkMutationActionField actionName actionInfo definitionList kind =
  ( actionExecutionContext
  , fieldInfo
  )
  where
    definition = _aiDefinition actionInfo
    actionExecutionContext =
      case kind of
        ActionSynchronous  ->
          ActionExecutionSyncWebhook $ ActionExecutionContext actionName
          (_adOutputType definition)
          (_aiOutputFields actionInfo)
          definitionList
          (_adHandler definition)
          (_adHeaders definition)
          (_adForwardClientHeaders definition)
        ActionAsynchronous -> ActionExecutionAsync

    description = mkDescriptionWith (PGDescription <$> (_aiComment actionInfo)) $
                  "perform the action: " <>> actionName

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName $ map mkActionArgument $ _adArguments definition)
      actionFieldResponseType

    mkActionArgument argument =
      InpValInfo (_argDescription argument) (unArgumentName $ _argName argument)
      Nothing $ unGraphQLType $ _argType argument

    actionFieldResponseType =
      case kind of
        ActionSynchronous  -> unGraphQLType $ _adOutputType definition
        ActionAsynchronous -> G.toGT $ G.toNT $ mkScalarTy PGUUID

mkQueryField
  :: ActionName
  -> Maybe Text
  -> ResolvedActionDefinition
  -> [(PGCol, PGScalarType)]
  -> ActionMutationKind
  -> Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
mkQueryField actionName comment definition definitionList kind =
  case kind of
    ActionAsynchronous ->
      Just ( ActionSelectOpContext (_adOutputType definition) definitionList

           , mkHsraObjFldInfo (Just description) (unActionName actionName)
             (mapFromL _iviName [idArgument])
             (G.toGT $ G.toGT $ mkAsyncActionSelectionType actionName)

           , TIObj $ mkAsyncActionQueryResponseObj actionName $
             _adOutputType definition
           )
    ActionSynchronous -> Nothing
  where
    description = mkDescriptionWith (PGDescription <$> comment) $
                  "retrieve the result of action: " <>> actionName

    idArgument =
      InpValInfo (Just idDescription) "id" Nothing $ G.toNT $ mkScalarTy PGUUID
      where
        idDescription = G.Description $ "id of the action: " <>> actionName

mkPGFieldType
  :: ObjectFieldName
  -> (G.GType, OutputFieldTypeInfo)
  -> HashMap ObjectFieldName PGColumnInfo
  -> PGScalarType
mkPGFieldType fieldName (fieldType, fieldTypeInfo) fieldReferences =
  case (G.isListType fieldType, fieldTypeInfo) of
  -- for scalar lists, we treat them as json columns
    (True, _) -> PGJSON
    -- enums the same
    (False, OutputFieldEnum _) -> PGJSON
    -- default to PGJSON unless you have to join with a postgres table
    -- i.e, if this field is specified as part of some relationship's
    -- mapping, we can cast this column's value as the remote column's type
    (False, OutputFieldScalar _) ->
      case Map.lookup fieldName fieldReferences of
        Just columnInfo -> unsafePGColumnToRepresentation $ pgiType columnInfo
        Nothing         -> PGJSON


mkDefinitionList :: AnnotatedObjectType -> HashMap ObjectFieldName PGColumnInfo -> [(PGCol, PGScalarType)]
mkDefinitionList annotatedOutputType fieldReferences =
  [ (unsafePGCol $ coerce k, mkPGFieldType k v fieldReferences)
  | (k, v) <- Map.toList $ _aotAnnotatedFields annotatedOutputType
  ]

mkFieldMap
  :: AnnotatedObjectType
  -> ActionInfo
  -> HashMap ObjectFieldName PGColumnInfo
  -> RoleName
  -> HashMap (G.NamedType,G.Name) ResolveField
mkFieldMap annotatedOutputType actionInfo fieldReferences roleName =
  Map.fromList $ fields <> catMaybes relationships
  where
    fields =
      flip map (Map.toList $ _aotAnnotatedFields annotatedOutputType) $
      \(fieldName, (fieldType, fieldTypeInfo)) ->
        ( (actionOutputBaseType, unObjectFieldName fieldName)
        , RFPGColumn $ PGColumnInfo
          (unsafePGCol $ coerce fieldName)
          (coerce fieldName)
          0
          (PGColumnScalar $ mkPGFieldType fieldName (fieldType, fieldTypeInfo) fieldReferences)
          (G.isNullable fieldType)
          Nothing
        )
    relationships =
      flip map (Map.toList $ _aotRelationships annotatedOutputType) $
      \(relationshipName, relationship) ->
        let remoteTableInfo = _trRemoteTable relationship
            remoteTable = _tciName $ _tiCoreInfo remoteTableInfo
            filterAndLimitM = getFilterAndLimit remoteTableInfo
            columnMapping = Map.fromList $
                  [ (unsafePGCol $ coerce k, pgiColumn v)
                  | (k, v) <- Map.toList $ _trFieldMapping relationship
                  ]
            in case filterAndLimitM of
              Just (tableFilter, tableLimit) ->
                Just ( ( actionOutputBaseType
                       , unRelationshipName relationshipName
                       )
                     , RFRelationship $ RelationshipField
                       (RelInfo
                        -- RelationshipName, which is newtype wrapper over G.Name is always
                        -- non-empty text so as to conform GraphQL spec
                        (RelName $ mkNonEmptyTextUnsafe $ coerce relationshipName)
                        (_trType relationship)
                        columnMapping remoteTable True)
                       False mempty
                       tableFilter
                       tableLimit
                     )
              Nothing -> Nothing

    getFilterAndLimit remoteTableInfo =
      if roleName == adminRole
      then Just (annBoolExpTrue, Nothing)
      else do
        selectPermisisonInfo <-
          getSelectPermissionInfoM remoteTableInfo roleName
        return (spiFilter selectPermisisonInfo, spiLimit selectPermisisonInfo)

    actionOutputBaseType =
      G.getBaseType $ unGraphQLType $ _adOutputType $ _aiDefinition actionInfo

mkFieldReferences :: AnnotatedObjectType -> HashMap ObjectFieldName PGColumnInfo
mkFieldReferences annotatedOutputType=
  Map.unions $ map _trFieldMapping $ Map.elems $
  _aotRelationships annotatedOutputType

mkMutationActionFieldsAndTypes
  :: (QErrM m)
  => ActionInfo
  -> AnnotatedObjectType
  -> ActionPermissionInfo
  -> ActionMutationKind
  -> m ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
       -- context, field, response type info
     , (ActionMutationExecutionContext, ObjFldInfo) -- mutation field
     , FieldMap
     )
mkMutationActionFieldsAndTypes actionInfo annotatedOutputType permission kind =
  return ( mkQueryField actionName comment definition definitionList kind
         , mkMutationActionField actionName actionInfo definitionList kind
         , fieldMap
         )
  where
    actionName = _aiName actionInfo
    definition = _aiDefinition actionInfo
    roleName = _apiRole permission
    comment = _aiComment actionInfo

    -- all the possible field references
    fieldReferences = mkFieldReferences annotatedOutputType

    definitionList = mkDefinitionList annotatedOutputType fieldReferences

    fieldMap = mkFieldMap annotatedOutputType actionInfo fieldReferences roleName

mkQueryActionFieldsAndTypes
  :: (QErrM m)
  => ActionInfo
  -> AnnotatedObjectType
  -> ActionPermissionInfo
  -> m ((ActionExecutionContext, ObjFldInfo)
     , FieldMap
     )
mkQueryActionFieldsAndTypes actionInfo annotatedOutputType permission =
  return ( mkQueryActionField actionName actionInfo definitionList
         , fieldMap
         )
  where
    actionName = _aiName actionInfo
    roleName = _apiRole permission

    fieldReferences = mkFieldReferences annotatedOutputType

    definitionList = mkDefinitionList annotatedOutputType fieldReferences

    fieldMap = mkFieldMap annotatedOutputType actionInfo fieldReferences roleName

mkMutationActionSchemaOne
  :: (QErrM m)
  => AnnotatedObjects
  -> ActionInfo
  -> ActionMutationKind
  -> m (Map.HashMap RoleName
         ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
         , (ActionMutationExecutionContext, ObjFldInfo)
         , FieldMap
         )
       )
mkMutationActionSchemaOne annotatedObjects actionInfo kind = do
  annotatedOutputType <- onNothing
      (Map.lookup (ObjectTypeName actionOutputBaseType) annotatedObjects) $
      throw500 $ "missing annotated type for: " <> showNamedTy actionOutputBaseType
  forM permissions $ \permission ->
    mkMutationActionFieldsAndTypes actionInfo annotatedOutputType permission kind
  where
    adminPermission = ActionPermissionInfo adminRole
    permissions = Map.insert adminRole adminPermission $ _aiPermissions actionInfo
    actionOutputBaseType =
      G.getBaseType $ unGraphQLType $ _adOutputType $ _aiDefinition actionInfo

mkQueryActionSchemaOne
  :: (QErrM m)
  => AnnotatedObjects
  -> ActionInfo
  -> m (Map.HashMap RoleName
         ( (ActionExecutionContext, ObjFldInfo)
         , FieldMap
         )
       )
mkQueryActionSchemaOne annotatedObjects actionInfo = do
  annotatedOutputType <- onNothing
      (Map.lookup (ObjectTypeName actionOutputBaseType) annotatedObjects) $
      throw500 $ "missing annotated type for: " <> showNamedTy actionOutputBaseType
  forM permissions $ \permission ->
    mkQueryActionFieldsAndTypes actionInfo annotatedOutputType permission
  where
    adminPermission = ActionPermissionInfo adminRole
    permissions = Map.insert adminRole adminPermission $ _aiPermissions actionInfo
    actionOutputBaseType =
      G.getBaseType $ unGraphQLType $ _adOutputType $ _aiDefinition actionInfo

mkActionsSchema
  :: (QErrM m)
  => AnnotatedObjects
  -> ActionCache
  -> m (Map.HashMap RoleName (RootFields, TyAgg))
mkActionsSchema annotatedObjects =
  foldM
  (\aggregate actionInfo ->
     case _adType $ _aiDefinition actionInfo of
       ActionQuery ->
         Map.foldrWithKey fQuery aggregate <$> mkQueryActionSchemaOne annotatedObjects actionInfo
       ActionMutation kind ->
         Map.foldrWithKey fMutation aggregate <$> mkMutationActionSchemaOne annotatedObjects actionInfo kind
  )
  mempty
  where
    -- we'll need to add uuid and timestamptz for actions
    newRoleState = (mempty, addScalarToTyAgg PGJSON $
                            addScalarToTyAgg PGTimeStampTZ $
                            addScalarToTyAgg PGUUID mempty)
    fQuery roleName (actionField, fields) =
      Map.alter (Just . addToStateSync . fromMaybe newRoleState) roleName
      where
        addToStateSync (rootFields, tyAgg) =
          ( addQueryField (first QCAction actionField) rootFields
          , addFieldsToTyAgg fields tyAgg
          )

    fMutation roleName (queryFieldM, actionField, fields) =
      Map.alter (Just . addToState . fromMaybe newRoleState) roleName
      where
        addToState = case queryFieldM of
          Just (fldCtx, fldDefinition, responseTypeInfo) ->
            addToStateAsync (fldCtx, fldDefinition) responseTypeInfo
          Nothing -> addToStateSync
        addToStateSync (rootFields, tyAgg) =
          ( addMutationField (first MCAction actionField) rootFields
          , addFieldsToTyAgg fields tyAgg
          )
        addToStateAsync queryField responseTypeInfo (rootFields, tyAgg) =
          ( addMutationField (first MCAction actionField) $
            addQueryField
            (first QCAsyncActionFetch queryField)
            rootFields
          , addTypeInfoToTyAgg responseTypeInfo $
            addFieldsToTyAgg fields tyAgg
          )
