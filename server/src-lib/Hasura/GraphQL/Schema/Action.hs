module Hasura.GraphQL.Schema.Action
  ( mkActionsSchema
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Builder
import           Hasura.GraphQL.Schema.Common  (mkDescriptionWith)

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Session
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
      (getActionOutputFields $ _aiOutputObject actionInfo)
      definitionList
      (_adHandler definition)
      (_adHeaders definition)
      (_adForwardClientHeaders definition)

    description = mkDescriptionWith (PGDescription <$> _aiComment actionInfo) $
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
          ActionMutationSyncWebhook $ ActionExecutionContext actionName
          (_adOutputType definition)
          (getActionOutputFields $ _aiOutputObject actionInfo)
          definitionList
          (_adHandler definition)
          (_adHeaders definition)
          (_adForwardClientHeaders definition)
        ActionAsynchronous -> ActionMutationAsync

    description = mkDescriptionWith (PGDescription <$> _aiComment actionInfo) $
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
                       RFKSimple mempty
                       tableFilter
                       tableLimit
                     )
              Nothing -> Nothing

    getFilterAndLimit remoteTableInfo =
      if roleName == adminRoleName
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
  :: ActionInfo
  -> ActionPermissionInfo
  -> ActionMutationKind
  -> ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
       -- context, field, response type info
     , (ActionMutationExecutionContext, ObjFldInfo) -- mutation field
     , FieldMap
     )
mkMutationActionFieldsAndTypes actionInfo permission kind =
  ( mkQueryField actionName comment definition definitionList kind
  , mkMutationActionField actionName actionInfo definitionList kind
  , fieldMap
  )
  where
    actionName = _aiName actionInfo
    annotatedOutputType = _aiOutputObject actionInfo
    definition = _aiDefinition actionInfo
    roleName = _apiRole permission
    comment = _aiComment actionInfo

    -- all the possible field references
    fieldReferences = mkFieldReferences annotatedOutputType

    definitionList = mkDefinitionList annotatedOutputType fieldReferences

    fieldMap = mkFieldMap annotatedOutputType actionInfo fieldReferences roleName

mkQueryActionFieldsAndTypes
  :: ActionInfo
  -> ActionPermissionInfo
  -> ((ActionExecutionContext, ObjFldInfo)
     , FieldMap
     )
mkQueryActionFieldsAndTypes actionInfo permission =
  ( mkQueryActionField actionName actionInfo definitionList
  , fieldMap
  )
  where
    actionName = _aiName actionInfo
    roleName = _apiRole permission
    annotatedOutputType = _aiOutputObject actionInfo

    fieldReferences = mkFieldReferences annotatedOutputType

    definitionList = mkDefinitionList annotatedOutputType fieldReferences

    fieldMap = mkFieldMap annotatedOutputType actionInfo fieldReferences roleName

mkMutationActionSchemaOne
  :: ActionInfo
  -> ActionMutationKind
  -> Map.HashMap RoleName
         ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
         , (ActionMutationExecutionContext, ObjFldInfo)
         , FieldMap
         )
mkMutationActionSchemaOne actionInfo kind =
  flip Map.map permissions $ \permission ->
    mkMutationActionFieldsAndTypes actionInfo permission kind
  where
    adminPermission = ActionPermissionInfo adminRoleName
    permissions = Map.insert adminRoleName adminPermission $ _aiPermissions actionInfo

mkQueryActionSchemaOne
  :: ActionInfo
  -> Map.HashMap RoleName
         ( (ActionExecutionContext, ObjFldInfo)
         , FieldMap
         )
mkQueryActionSchemaOne actionInfo =
  flip Map.map permissions $ \permission ->
    mkQueryActionFieldsAndTypes actionInfo permission
  where
    adminPermission = ActionPermissionInfo adminRoleName
    permissions = Map.insert adminRoleName adminPermission $ _aiPermissions actionInfo

mkActionsSchema
  :: ActionCache
  -> Map.HashMap RoleName (RootFields, TyAgg)
mkActionsSchema =
  foldl'
  (\aggregate actionInfo ->
     case _adType $ _aiDefinition actionInfo of
       ActionQuery ->
         Map.foldrWithKey (accumulateQuery (_aiPgScalars actionInfo)) aggregate $
           mkQueryActionSchemaOne actionInfo
       ActionMutation kind ->
         Map.foldrWithKey (accumulateMutation (_aiPgScalars actionInfo)) aggregate $
           mkMutationActionSchemaOne actionInfo kind
  )
  mempty
  where
    -- we'll need to add uuid and timestamptz for actions
    initRoleState =
      ( mempty
      , foldr addScalarToTyAgg mempty [PGJSON, PGTimeStampTZ, PGUUID]
      )

    addScalarsToTyAgg = foldr addScalarToTyAgg

    accumulateQuery pgScalars roleName (actionField, fields) =
      Map.alter (Just . addToStateSync . fromMaybe initRoleState) roleName
      where
        addToStateSync (rootFields, tyAgg) =
          ( addQueryField (first QCAction actionField) rootFields
          -- Add reused PG scalars to TyAgg
          , addFieldsToTyAgg fields $ addScalarsToTyAgg tyAgg pgScalars
          )

    accumulateMutation pgScalars roleName (queryFieldM, actionField, fields) =
      Map.alter (Just . addToState . fromMaybe initRoleState) roleName
      where
        addToState (rootFields, tyAgg) =
          let rootFieldsWithActionMutation =
                addMutationField (first MCAction actionField) rootFields
              -- Add reused PG scalars to TyAgg
              tyAggWithFieldsAndPgScalars =
                addFieldsToTyAgg fields $ addScalarsToTyAgg tyAgg pgScalars
          in case queryFieldM of
               Just (fldCtx, fldDefinition, responseTypeInfo) ->
                 -- Add async action's query resolver and response type
                 ( addQueryField (QCAsyncActionFetch fldCtx, fldDefinition)
                   rootFieldsWithActionMutation
                 , addTypeInfoToTyAgg responseTypeInfo tyAggWithFieldsAndPgScalars
                 )
               Nothing -> (rootFieldsWithActionMutation, tyAggWithFieldsAndPgScalars)
