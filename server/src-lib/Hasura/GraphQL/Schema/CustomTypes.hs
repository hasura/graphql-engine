module Hasura.GraphQL.Schema.CustomTypes
  ( buildCustomTypesSchemaPartial
  , buildCustomTypesSchema
  ) where

import           Control.Lens

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Context        (defaultTypes)
import           Hasura.GraphQL.Schema.Common  (mkTableTy)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Validate.Types as VT

buildObjectTypeInfo :: RoleName -> AnnotatedObjectType -> VT.ObjTyInfo
buildObjectTypeInfo roleName annotatedObjectType =
  let description = _otdDescription objectDefinition
      namedType = unObjectTypeName $ _otdName objectDefinition
      fieldMap = mapFromL VT._fiName $ fields <> catMaybes relationships
  -- 'mkObjTyInfo' function takes care of inserting `__typename` field
  in VT.mkObjTyInfo description namedType mempty fieldMap VT.TLCustom
  where
    objectDefinition = _aotDefinition annotatedObjectType

    relationships =
      flip map (toList $ _aotRelationships annotatedObjectType) $
      \(TypeRelationship name ty remoteTableInfo _) ->
        if isJust (getSelectPermissionInfoM remoteTableInfo roleName) ||
           roleName == adminRoleName
        then Just (relationshipToFieldInfo name ty $ _tciName $ _tiCoreInfo remoteTableInfo)
        else Nothing
      where
        relationshipToFieldInfo name relTy remoteTableName =
          let fieldTy = case relTy of
                ObjRel -> G.toGT $ mkTableTy remoteTableName
                ArrRel -> G.toGT $ G.toLT $ mkTableTy remoteTableName
          in VT.ObjFldInfo
             { VT._fiDesc = Nothing -- TODO
             , VT._fiName = unRelationshipName name
             , VT._fiParams = mempty
             , VT._fiTy = fieldTy
             , VT._fiLoc = VT.TLCustom
             }

    fields =
      map convertObjectFieldDefinition $
      toList $ _otdFields objectDefinition
      where
        convertObjectFieldDefinition fieldDefinition =
          VT.ObjFldInfo
          { VT._fiDesc = _ofdDescription fieldDefinition
          , VT._fiName = unObjectFieldName $ _ofdName fieldDefinition
          , VT._fiParams = mempty
          , VT._fiTy = unGraphQLType $ _ofdType fieldDefinition
          , VT._fiLoc = VT.TLCustom
          }

buildCustomTypesSchema
  :: NonObjectTypeMap -> AnnotatedObjects -> RoleName -> VT.TypeMap
buildCustomTypesSchema nonObjectTypeMap annotatedObjectTypes roleName =
  unNonObjectTypeMap nonObjectTypeMap <> objectTypeInfos
  where
    objectTypeInfos =
      mapFromL VT.getNamedTy $
      map (VT.TIObj . buildObjectTypeInfo roleName) $
      Map.elems annotatedObjectTypes

annotateObjectType
  :: (MonadError QErr m)
  => TableCache -> NonObjectTypeMap -> ObjectTypeDefinition -> m AnnotatedObjectType
annotateObjectType tableCache nonObjectTypeMap objectDefinition = do
  annotatedFields <-
    fmap Map.fromList $ forM (toList $ _otdFields objectDefinition) $
    \objectField -> do
    let fieldName = _ofdName objectField
        fieldType = unGraphQLType $ _ofdType objectField
        fieldBaseType = G.getBaseType fieldType
    baseTypeInfo <- getFieldTypeInfo fieldBaseType
    return (fieldName, (fieldType, baseTypeInfo))
  annotatedRelationships <-
    fmap Map.fromList $ forM relationships $
    \relationship -> do
      let relationshipName = _trName relationship
          remoteTable = _trRemoteTable relationship
      remoteTableInfo <- onNothing (Map.lookup remoteTable tableCache) $
        throw500 $ "missing table info for: " <>> remoteTable
      annotatedFieldMapping <-
        forM (_trFieldMapping relationship) $ \remoteTableColumn -> do
        let fieldName = fromPGCol remoteTableColumn
        onNothing (getPGColumnInfoM remoteTableInfo fieldName) $
          throw500 $ "missing column info of " <> fieldName
          <<> " in table" <>> remoteTable
      return ( relationshipName
             , relationship & trRemoteTable .~ remoteTableInfo
               & trFieldMapping .~ annotatedFieldMapping)
  return $ AnnotatedObjectType objectDefinition
    annotatedFields annotatedRelationships
  where
    relationships = fromMaybe [] $ _otdRelationships objectDefinition
    getFieldTypeInfo typeName = do
      let inputTypeInfos = unNonObjectTypeMap nonObjectTypeMap
                           <> mapFromL VT.getNamedTy defaultTypes
      typeInfo <- onNothing (Map.lookup typeName inputTypeInfos) $
        throw500 $ "the type: " <> VT.showNamedTy typeName <>
        " is not found in non-object cutom types"
      case typeInfo of
        VT.TIScalar scalarTypeInfo -> return $ OutputFieldScalar scalarTypeInfo
        VT.TIEnum enumTypeInfo -> return $ OutputFieldEnum enumTypeInfo
        _ -> throw500 $
             "expecting only scalar/enum typeinfo for an object type's field: " <>
             VT.showNamedTy typeName

buildCustomTypesSchemaPartial
  :: (QErrM m)
  => TableCache -> CustomTypes -> m (NonObjectTypeMap, AnnotatedObjects)
buildCustomTypesSchemaPartial tableCache customTypes = do
  let typeInfos =
        map (VT.TIEnum . convertEnumDefinition) enumDefinitions <>
        -- map (VT.TIObj . convertObjectDefinition) objectDefinitions <>
        map (VT.TIInpObj . convertInputObjectDefinition) inputObjectDefinitions <>
        map (VT.TIScalar . convertScalarDefinition) scalarDefinitions
        -- <> defaultTypes
      nonObjectTypeMap = NonObjectTypeMap $ mapFromL VT.getNamedTy typeInfos

  annotatedObjectTypes <- mapFromL (_otdName . _aotDefinition) <$>
    traverse (annotateObjectType tableCache nonObjectTypeMap) objectDefinitions

  return (nonObjectTypeMap, annotatedObjectTypes)
  where
    inputObjectDefinitions = fromMaybe [] $ _ctInputObjects customTypes
    objectDefinitions = fromMaybe [] $ _ctObjects customTypes
    scalarDefinitions = fromMaybe [] $ _ctScalars customTypes
    enumDefinitions = fromMaybe [] $ _ctEnums customTypes

    convertScalarDefinition scalarDefinition =
      flip VT.fromScalarTyDef VT.TLCustom $ G.ScalarTypeDefinition
      (_stdDescription scalarDefinition)
      (G.unNamedType $ _stdName scalarDefinition) mempty

    convertEnumDefinition enumDefinition =
      VT.EnumTyInfo (_etdDescription enumDefinition)
      (unEnumTypeName $ _etdName enumDefinition)
      (VT.EnumValuesSynthetic $ mapFromL VT._eviVal $
       map convertEnumValueDefinition $ toList $ _etdValues enumDefinition)
      VT.TLCustom
      where
        convertEnumValueDefinition enumValueDefinition =
          VT.EnumValInfo (_evdDescription enumValueDefinition)
          (_evdValue enumValueDefinition)
          (fromMaybe False $ _evdIsDeprecated enumValueDefinition)

    convertInputObjectDefinition inputObjectDefinition =
      VT.InpObjTyInfo
      { VT._iotiDesc = _iotdDescription inputObjectDefinition
      , VT._iotiName = unInputObjectTypeName $ _iotdName inputObjectDefinition
      , VT._iotiFields = mapFromL VT._iviName $ map convertInputFieldDefinition $
                         toList $ _iotdFields inputObjectDefinition
      , VT._iotiLoc = VT.TLCustom
      }
      where
        convertInputFieldDefinition fieldDefinition =
          VT.InpValInfo
          { VT._iviDesc   = _iofdDescription fieldDefinition
          , VT._iviName   = unInputObjectFieldName $ _iofdName fieldDefinition
          , VT._iviDefVal = Nothing
          , VT._iviType   = unGraphQLType $ _iofdType fieldDefinition
          }
