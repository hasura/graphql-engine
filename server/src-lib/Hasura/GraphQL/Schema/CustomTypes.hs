module Hasura.GraphQL.Schema.CustomTypes
  ( AnnotatedRelationship(..)
  , buildCustomTypesSchemaPartial
  , buildCustomTypesSchema
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Context        (defaultTypes)
import           Hasura.GraphQL.Schema.Common  (mkTableTy)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Validate.Types as VT

buildObjectTypeInfo :: RoleName -> AnnotatedObjectType -> VT.ObjTyInfo
buildObjectTypeInfo roleName annotatedObjectType =
    VT.ObjTyInfo
    { VT._otiDesc = _otdDescription objectDefinition
    , VT._otiName = unObjectTypeName $ _otdName objectDefinition
    , VT._otiImplIFaces = mempty
    , VT._otiFields = VT.mapFromL VT._fiName $ fields <> catMaybes relationships
    }
  where
    objectDefinition = _aotDefinition annotatedObjectType

    relationships =
      flip map (toList $ _aotRelationships annotatedObjectType) $
      \(AnnotatedRelationship definition remoteTableInfo) ->
        if isJust (getSelectPermissionInfoM remoteTableInfo roleName) ||
           roleName == adminRole
        then Just (relationshipToFieldInfo definition)
        else Nothing
      where
        relationshipToFieldInfo relationship =
          VT.ObjFldInfo
          { VT._fiDesc = Nothing -- TODO
          , VT._fiName = unObjectRelationshipName $ _ordName relationship
          , VT._fiParams = mempty
          , VT._fiTy = G.toGT $ mkTableTy $ _ordRemoteTable relationship
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
      VT.mapFromL VT.getNamedTy $
      map (VT.TIObj . buildObjectTypeInfo roleName) $
      Map.elems annotatedObjectTypes

annotateObjectType
  :: (CacheRM m, MonadError QErr m)
  => NonObjectTypeMap -> ObjectTypeDefinition -> m AnnotatedObjectType
annotateObjectType nonObjectTypeMap objectDefinition = do
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
      let relationshipName = _ordName relationship
          remoteTable = _ordRemoteTable relationship
      remoteTableInfoM <- askTabInfoM remoteTable
      remoteTableInfo <- onNothing remoteTableInfoM $
        throw500 $ "missing table info for: " <>> remoteTable
      return ( relationshipName
             , AnnotatedRelationship relationship remoteTableInfo)
  return $ AnnotatedObjectType objectDefinition
    annotatedFields annotatedRelationships
  where
    relationships = fromMaybe [] $ _otdRelationships objectDefinition
    getFieldTypeInfo typeName = do
      let inputTypeInfos = unNonObjectTypeMap nonObjectTypeMap
                           <> VT.mapFromL VT.getNamedTy defaultTypes
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
  :: (CacheRM m, QErrM m)
  => CustomTypes -> m (NonObjectTypeMap, AnnotatedObjects)
buildCustomTypesSchemaPartial customTypes = do
  let typeInfos =
        map (VT.TIEnum . convertEnumDefinition) enumDefinitions <>
        -- map (VT.TIObj . convertObjectDefinition) objectDefinitions <>
        map (VT.TIInpObj . convertInputObjectDefinition) inputObjectDefinitions <>
        map (VT.TIScalar . convertScalarDefinition) scalarDefinitions
        -- <> defaultTypes
      nonObjectTypeMap = NonObjectTypeMap $ VT.mapFromL VT.getNamedTy typeInfos

  annotatedObjectTypes <- VT.mapFromL (_otdName . _aotDefinition) <$>
    traverse (annotateObjectType nonObjectTypeMap) objectDefinitions

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
      (VT.EnumValuesSynthetic $ VT.mapFromL VT._eviVal $
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
      , VT._iotiFields = VT.mapFromL VT._iviName $ map convertInputFieldDefinition $
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
