module Hasura.GraphQL.Schema.Action
  ( mkActionsSchema
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Builder
-- import qualified Data.HashSet                as Set

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

-- mkOutputSelectionTypeName :: ActionName -> G.Name
-- mkOutputSelectionTypeName actionName =
--   unActionName actionName <> "_output_selection"

-- mkOutputSelectionType :: ActionName -> G.NamedType
-- mkOutputSelectionType =
--   G.NamedType . mkOutputSelectionTypeName

-- mkOutputSelectionTypeInfo
--   :: ActionName
--   -- Name of the action
--   -> ActionOutputFieldTypes
--   -- allowed response columns
--   -> ObjTyInfo
-- mkOutputSelectionTypeInfo actionName allowedOutputFields =
--   mkHsraObjTyInfo
--   (Just description)
--   (mkOutputSelectionType actionName) -- "(action_name)_output"
--   mempty -- no arguments
--   (mapFromL _fiName outputFieldDefinitions)
--   where
--     description = G.Description $ "output fields of action: " <>> actionName
--     outputFieldDefinitions =
--       map (uncurry outputFieldToGqlField) $ Map.toList allowedOutputFields

    -- outputFieldToGqlField :: ActionOutputField -> PGColType -> ObjFldInfo
    -- outputFieldToGqlField fieldName fieldType =
    --   mkHsraObjFldInfo
    --   Nothing
    --   (unActionOutputField fieldName)
    --   mempty
    --   (G.toGT $ mkScalarTy fieldType)

-- mkInputSelectionTypeName :: ActionName -> G.Name
-- mkInputSelectionTypeName actionName =
--   unActionName actionName <> "_input_selection"

-- mkInputSelectionType :: ActionName -> G.NamedType
-- mkInputSelectionType =
--   G.NamedType . mkInputSelectionTypeName

-- mkInputSelectionTypeInfo
--   :: ActionName
--   -- Name of the action
--   -> ActionInputFieldTypes
--   -- input columns that are allowed to be read
--   -> ObjTyInfo
-- mkInputSelectionTypeInfo actionName allowedInputFields =
--   mkHsraObjTyInfo
--   (Just description)
--   (mkInputSelectionType actionName) -- "(action_name)_input"
--   mempty -- no arguments
--   (mapFromL _fiName inputFieldDefinitions)
--   where
--     description = G.Description $ "input fields of action: " <>> actionName
--     inputFieldDefinitions =
--       map (uncurry inputFieldToGqlField) $ Map.toList allowedInputFields

--     inputFieldToGqlField :: ActionInputField -> PGColType -> ObjFldInfo
--     inputFieldToGqlField fieldName fieldType =
--       mkHsraObjFldInfo
--       Nothing
--       (unActionInputField fieldName)
--       mempty
--       (G.toGT $ mkScalarTy fieldType)

mkActionSelectionType :: ActionName -> G.NamedType
mkActionSelectionType =
  G.NamedType . unActionName

mkActionResponseTypeInfo
  :: ActionName
  -- Name of the action
  -> GraphQLType
  -- output type
  -> ObjTyInfo
mkActionResponseTypeInfo actionName outputType =
  mkHsraObjTyInfo
  (Just description)
  (mkActionSelectionType actionName) -- "(action_name)_input"
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
      -- , ( "status", "the status of this action, whether it is processed, etc."
      --   , G.toGT $ G.NamedType "action_status")
      , ( "output", "the output fields of this action"
        , unGraphQLType outputType)
      ]

-- mkActionInputType :: ActionName -> G.NamedType
-- mkActionInputType actionName =
--   G.NamedType $ unActionName actionName <> "_input"

-- makes the input type for the allowed fields
-- mkInputTypeInfo
--   :: ActionName
--   -- Name of the action
--   -> ActionInputFieldTypes
--   -> InpObjTyInfo
-- mkInputTypeInfo actionName allowedInputFields =
--   mkHsraInpTyInfo
--   (Just description)
--   (mkActionInputType actionName)
--   inputFields
--   where
--     description =
--       G.Description $ "input arguments for action: " <>> actionName

--     inputFields =
--       mapFromL _iviName $
--       map (uncurry mkInputField) $ Map.toList allowedInputFields

--     mkInputField :: ActionInputField -> PGColType -> InpValInfo
--     mkInputField inputField ty =
--       InpValInfo
--       Nothing
--       (unActionInputField inputField)
--       Nothing -- no default value
--       (G.toGT $ mkScalarTy ty)

mkMutationField
  :: ActionName
  -> ActionInfo
  -> ActionPermissionInfo
  -> (ActionExecutionContext, ObjFldInfo)
mkMutationField actionName actionInfo permission =
  ( actionExecutionContext
  , fieldInfo
  )
  where
    definition = _aiDefintion actionInfo
    actionExecutionContext =
      case getActionKind definition of
        ActionSynchronous  ->
          ActionExecutionSyncWebhook
          (_adWebhook definition)
          (_aiOutputTypeInfo actionInfo)
        ActionAsynchronous -> ActionExecutionAsync $ _apiFilter permission

    -- TODO: we need to capture the comment from action definition
    description =
      G.Description $ "perform the action: " <>> actionName

    inputType = _adInputType definition

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName [inputArgument]) $
      actionFieldResponseType actionName definition

    inputArgument =
      InpValInfo (Just inputDescription) "input" Nothing $
      unGraphQLType inputType
      where
        inputDescription = G.Description $ "input for action: " <>> actionName

actionFieldResponseType :: ActionName -> ActionDefinition a -> G.GType
actionFieldResponseType actionName definition =
  case getActionKind definition of
    ActionSynchronous  -> unGraphQLType $ _adOutputType definition
    ActionAsynchronous -> G.toGT $ G.toGT $ mkActionSelectionType actionName

mkQueryField
  :: ActionName
  -> ResolvedActionDefinition
  -> ActionPermissionInfo
  -> Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
mkQueryField actionName definition permission =
  case getActionKind definition of
    ActionAsynchronous ->
      Just ( ActionSelectOpContext $ _apiFilter permission
           , fieldInfo
           , TIObj $ mkActionResponseTypeInfo actionName $
             _adOutputType definition
           )
    ActionSynchronous -> Nothing
  where
    -- TODO: we need to capture the comment from action definition
    description =
      G.Description $ "retrieve the result of action: " <>> actionName

    idArgument =
      InpValInfo (Just idDescription) "id" Nothing $ G.toNT $ mkScalarTy PGUUID
      where
        idDescription = G.Description $ "id of the action: " <>> actionName

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName [idArgument])
      (actionFieldResponseType actionName definition)

mkActionFieldsAndTypes
  :: ActionName
  -> ActionInfo
  -> ActionPermissionInfo
  -> ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
       -- context, field, response type info
     , (ActionExecutionContext, ObjFldInfo) -- mutation field
     )
mkActionFieldsAndTypes actionName actionInfo permission =
  ( mkQueryField actionName definition permission
  , mkMutationField actionName actionInfo permission
  )
  where
    definition = _aiDefintion actionInfo

mkActionSchemaOne
  :: ActionInfo
  -> Map.HashMap RoleName
  ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
  , (ActionExecutionContext, ObjFldInfo)
  )
mkActionSchemaOne actionInfo =
  flip fmap permissions $ \permission ->
  mkActionFieldsAndTypes (_aiName actionInfo) actionInfo permission
  where
    adminPermission = ActionPermissionInfo adminRole annBoolExpTrue
    permissions = Map.insert adminRole adminPermission $ _aiPermissions actionInfo

mkActionsSchema
  :: ActionCache
  -> Map.HashMap RoleName (RootFields, TyAgg)
mkActionsSchema =
  foldr (\actionInfo aggregate ->
           Map.foldrWithKey f aggregate $ mkActionSchemaOne actionInfo)
  mempty
  where
    -- we'll need to add uuid and timestamptz for actions
    newRoleState = (mempty, addScalarToTyAgg PGTimeStampTZ $
                            addScalarToTyAgg PGUUID mempty)
    f roleName (queryFieldM, mutationField) =
      Map.alter (Just . addToState . fromMaybe newRoleState) roleName
      where
        addToState = case queryFieldM of
          Just (fldCtx, fldDefinition, responseTypeInfo) ->
            addToStateAsync (fldCtx, fldDefinition) responseTypeInfo
          Nothing -> addToStateSync
        addToStateSync (rootFields, tyAgg) =
          ( addMutationField (first MCAction mutationField) rootFields
          , tyAgg
          )
        addToStateAsync queryField responseTypeInfo (rootFields, tyAgg) =
          ( addMutationField (first MCAction mutationField) $
            addQueryField
            (first QCActionFetch queryField)
            rootFields
          , addTypeInfoToTyAgg responseTypeInfo tyAgg
          )
