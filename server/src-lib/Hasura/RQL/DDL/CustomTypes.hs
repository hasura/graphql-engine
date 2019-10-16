module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes
  , runSetCustomTypes_
  , clearCustomTypes
  , validateCustomTypesAndAddToCache
  ) where

import           Control.Monad.Validate

import qualified Data.HashSet                  as Set
import qualified Data.List.Extended            as L
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Validate.Types as VT

validateCustomTypeDefinitions
  :: (MonadValidate [CustomTypeValidationError] m)
  => CustomTypes -> m ()
validateCustomTypeDefinitions customTypes = do
  unless (null duplicateTypes) $ dispute $ pure $ DuplicateTypeNames duplicateTypes
  traverse_ validateEnum enumDefinitions
  traverse_ validateInputObject inputObjectDefinitions
  traverse_ validateObject objectDefinitions
  where
    inputObjectDefinitions = fromMaybe [] $ _ctInputObjects customTypes
    objectDefinitions = fromMaybe [] $ _ctObjects customTypes
    scalarDefinitions = fromMaybe [] $ _ctScalars customTypes
    enumDefinitions = fromMaybe [] $ _ctEnums customTypes

    duplicateTypes = L.duplicates allTypes
    allTypes =
      map _stdName scalarDefinitions <>
      map (unEnumTypeName . _etdName) enumDefinitions <>
      map (unInputObjectTypeName . _iotdName) inputObjectDefinitions <>
      map (unObjectTypeName . _otdName) objectDefinitions

    -- TODO: add default types
    scalarAndEnumTypes =
      Set.fromList $
      map _stdName scalarDefinitions <> defaultScalars <>
      map (unEnumTypeName . _etdName) enumDefinitions

    -- TODO
    defaultScalars = map G.NamedType ["Int", "Float", "String", "Boolean"]

    validateEnum
      :: (MonadValidate [CustomTypeValidationError] m)
      => EnumTypeDefinition -> m ()
    validateEnum enumDefinition = do
      let duplicateEnumValues = L.duplicates $ map _evdValue $ toList $
                                _etdValues enumDefinition
      -- check for duplicate field names
      unless (null duplicateEnumValues) $
        dispute $ pure $ DuplicateEnumValues
        (_etdName enumDefinition) duplicateEnumValues

    validateInputObject
      :: (MonadValidate [CustomTypeValidationError] m)
      => InputObjectTypeDefinition -> m ()
    validateInputObject inputObjectDefinition = do
      let inputObjectTypeName = _iotdName inputObjectDefinition
          duplicateFieldNames =
            L.duplicates $ map _iofdName $ toList $
            _iotdFields inputObjectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ InputObjectDuplicateFields
        inputObjectTypeName duplicateFieldNames

      let inputTypes = scalarAndEnumTypes `Set.union` Set.fromList
                       (map (unInputObjectTypeName . _iotdName) inputObjectDefinitions)
      -- check that fields reference input types
      for_ (_iotdFields inputObjectDefinition) $
        \inputObjectField -> do
        let fieldBaseType = G.getBaseType $ unGraphQLType $ _iofdType inputObjectField
        unless (Set.member fieldBaseType inputTypes) $
          refute $ pure $ InputObjectFieldTypeDoesNotExist
          (_iotdName inputObjectDefinition)
          (_iofdName inputObjectField) fieldBaseType

    validateObject
      :: (MonadValidate [CustomTypeValidationError] m)
      => ObjectTypeDefinition -> m ()
    validateObject objectDefinition = do
      let objectTypeName = _otdName objectDefinition
          duplicateFieldNames =
            L.duplicates $ map _ofdName $ toList $ _otdFields objectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ ObjectDuplicateFields objectTypeName duplicateFieldNames

      for_ (_otdFields objectDefinition) $
        \objectField -> do
        let fieldBaseType = G.getBaseType $ unGraphQLType $ _ofdType objectField
            fieldName = _ofdName objectField

        -- check that arguments are not defined
        when (isJust $ _ofdArguments objectField) $
          dispute $ pure $ ObjectFieldArgumentsNotAllowed
          objectTypeName fieldName

        let objectTypes = Set.fromList $ map (unObjectTypeName . _otdName)
                          objectDefinitions
        -- check that the fields only reference scalars and enums
        -- and not other object types
        if | Set.member fieldBaseType scalarAndEnumTypes -> return ()
           | Set.member fieldBaseType objectTypes ->
               dispute $ pure $ ObjectFieldObjectBaseType
               objectTypeName fieldName fieldBaseType
           | otherwise ->
               dispute $ pure $ ObjectFieldTypeDoesNotExist
               objectTypeName fieldName fieldBaseType

data CustomTypeValidationError
  -- ^ type names have to be unique across all types
  = DuplicateTypeNames !(Set.HashSet G.NamedType)
  -- ^ field name and the field's base type
  | InputObjectFieldTypeDoesNotExist
    !InputObjectTypeName !InputObjectFieldName !G.NamedType
  -- ^ duplicate field declaration in input objects
  | InputObjectDuplicateFields
    !InputObjectTypeName !(Set.HashSet InputObjectFieldName)
  -- ^ field name and the field's base type
  | ObjectFieldTypeDoesNotExist
    !ObjectTypeName !ObjectFieldName !G.NamedType
  -- ^ duplicate field declaration in objects
  | ObjectDuplicateFields !ObjectTypeName !(Set.HashSet ObjectFieldName)
  -- ^ object fields can't have arguments
  | ObjectFieldArgumentsNotAllowed !ObjectTypeName !ObjectFieldName
  -- ^ object fields can't have object types as base types
  | ObjectFieldObjectBaseType !ObjectTypeName !ObjectFieldName !G.NamedType
  -- ^ duplicate enum values
  | DuplicateEnumValues !EnumTypeName !(Set.HashSet G.EnumValue)
  deriving (Show, Eq)

showCustomTypeValidationError
  :: CustomTypeValidationError -> T.Text
showCustomTypeValidationError =
  -- TODO
  T.pack . show

runSetCustomTypes
  :: ( MonadError QErr m
     , UserInfoM m
     , CacheRWM m
     , MonadTx m
     )
  => CustomTypes -> m EncJSON
runSetCustomTypes customTypes = do
  adminOnly
  runSetCustomTypes_ customTypes
  return successMsg

runSetCustomTypes_
  :: ( MonadError QErr m
     , CacheRWM m
     , MonadTx m
     )
  => CustomTypes -> m ()
runSetCustomTypes_ customTypes = do
  validateCustomTypesAndAddToCache customTypes
  liftTx $ persistCustomTypes customTypes

persistCustomTypes :: CustomTypes -> Q.TxE QErr ()
persistCustomTypes customTypes = do
  clearCustomTypes
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_custom_graphql_types
      (custom_types)
      VALUES ($1)
  |] (Identity $ Q.AltJ customTypes) False

clearCustomTypes :: Q.TxE QErr ()
clearCustomTypes = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.hdb_custom_graphql_types
  |] () False

validateCustomTypesAndAddToCache
  :: ( MonadError QErr m
     , CacheRWM m
     )
  => CustomTypes -> m ()
validateCustomTypesAndAddToCache customTypes = do
  either (throw400 ConstraintViolation . showErrors) pure
    =<< runValidateT (validateCustomTypeDefinitions customTypes)
  let typeInfos =
        map (VT.TIEnum . convertEnumDefinition) enumDefinitions <>
        map (VT.TIObj . convertObjectDefinition) objectDefinitions <>
        map (VT.TIInpObj . convertInputObjectDefinition) inputObjectDefinitions <>
        map (VT.TIScalar . convertScalarDefinition) scalarDefinitions
  setCustomTypesInCache $ VT.mapFromL VT.getNamedTy typeInfos
  where
    inputObjectDefinitions = fromMaybe [] $ _ctInputObjects customTypes
    objectDefinitions = fromMaybe [] $ _ctObjects customTypes
    scalarDefinitions = fromMaybe [] $ _ctScalars customTypes
    enumDefinitions = fromMaybe [] $ _ctEnums customTypes

    showErrors :: [CustomTypeValidationError] -> T.Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showCustomTypeValidationError) allErrors)

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

    convertObjectDefinition objectDefinition =
      VT.ObjTyInfo
      { VT._otiDesc = _otdDescription objectDefinition
      , VT._otiName = unObjectTypeName $ _otdName objectDefinition
      , VT._otiImplIFaces = mempty
      , VT._otiFields = VT.mapFromL VT._fiName $ map convertObjectFieldDefinition $
                        toList $ _otdFields objectDefinition
      }
      where
        convertObjectFieldDefinition fieldDefinition =
          VT.ObjFldInfo
          { VT._fiDesc = _ofdDescription fieldDefinition
          , VT._fiName = unObjectFieldName $ _ofdName fieldDefinition
          , VT._fiParams = mempty
          , VT._fiTy = unGraphQLType $ _ofdType fieldDefinition
          , VT._fiLoc = VT.TLCustom
          }

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
