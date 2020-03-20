module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes
  , persistCustomTypes
  , clearCustomTypes
  , resolveCustomTypes
  ) where

import           Control.Monad.Validate

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as Set
import qualified Data.List.Extended                as L
import qualified Data.Text                         as T
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

validateCustomTypeDefinitions
  :: (MonadValidate [CustomTypeValidationError] m)
  => TableCache -> CustomTypes -> m ()
validateCustomTypeDefinitions tableCache customTypes = do
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

    scalarTypes =
      Set.fromList $ map _stdName scalarDefinitions <> defaultScalars

    enumTypes =
      Set.fromList $ map (unEnumTypeName . _etdName) enumDefinitions

    -- TODO, clean it up maybe?
    defaultScalars =
      [ $$(G.litName "Int")
      , $$(G.litName "Float")
      , $$(G.litName "String")
      , $$(G.litName "Boolean") ]
      , $$(G.litName "ID") ]

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

      let inputObjectTypes =
            Set.fromList $ map (unInputObjectTypeName . _iotdName)
            inputObjectDefinitions

      let inputTypes =
            scalarTypes `Set.union` enumTypes `Set.union` inputObjectTypes

      -- check that fields reference input types
      for_ (_iotdFields inputObjectDefinition) $ \inputObjectField -> do
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
          fieldNames = map (unObjectFieldName . _ofdName) $
                       toList (_otdFields objectDefinition)
          relationships = fromMaybe [] $ _otdRelationships objectDefinition
          relNames = map (unRelationshipName . _trName) relationships
          duplicateFieldNames = L.duplicates $ fieldNames <> relNames
          fields = toList $ _otdFields objectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ ObjectDuplicateFields objectTypeName duplicateFieldNames

      scalarFields <- fmap (Map.fromList . catMaybes) $
        for fields $ \objectField -> do
        let fieldType = _ofdType objectField
            fieldBaseType = G.getBaseType $ unGraphQLType fieldType
            fieldName = _ofdName objectField

        -- check that arguments are not defined
        when (isJust $ _ofdArguments objectField) $
          dispute $ pure $ ObjectFieldArgumentsNotAllowed
          objectTypeName fieldName

        let objectTypes = Set.fromList $ map (unObjectTypeName . _otdName)
                          objectDefinitions

        -- check that the fields only reference scalars and enums
        -- and not other object types
        if | Set.member fieldBaseType scalarTypes -> return ()
           | Set.member fieldBaseType enumTypes -> return ()
           | Set.member fieldBaseType objectTypes ->
               dispute $ pure $ ObjectFieldObjectBaseType
               objectTypeName fieldName fieldBaseType
           | otherwise ->
               dispute $ pure $ ObjectFieldTypeDoesNotExist
               objectTypeName fieldName fieldBaseType

        -- collect all non list scalar types of this object
        if (not (isListType fieldType) && Set.member fieldBaseType scalarTypes)
          then pure $ Just (fieldName, fieldBaseType)
          else pure Nothing

      for_ relationships $ \relationshipField -> do
        let relationshipName = _trName relationshipField
            remoteTable = _trRemoteTable relationshipField
            fieldMapping = _trFieldMapping relationshipField

        --check that the table exists
        remoteTableInfo <- onNothing (Map.lookup remoteTable tableCache) $
          refute $ pure $ ObjectRelationshipTableDoesNotExist
          objectTypeName relationshipName remoteTable

        -- check that the column mapping is sane
        forM_ (Map.toList fieldMapping) $ \(fieldName, columnName) -> do

          -- the field should be a non-list type scalar
          when (Map.lookup fieldName scalarFields == Nothing) $
            dispute $ pure $ ObjectRelationshipFieldDoesNotExist
            objectTypeName relationshipName fieldName

          -- the column should be a column of the table
          when (getPGColumnInfoM remoteTableInfo (fromPGCol columnName) == Nothing) $
            dispute $ pure $ ObjectRelationshipColumnDoesNotExist
            objectTypeName relationshipName remoteTable columnName
          return ()

data CustomTypeValidationError
  = DuplicateTypeNames !(Set.HashSet G.Name)
  -- ^ type names have to be unique across all types
  | InputObjectFieldTypeDoesNotExist
    !InputObjectTypeName !InputObjectFieldName !G.Name
  -- ^ field name and the field's base type
  | InputObjectDuplicateFields
    !InputObjectTypeName !(Set.HashSet InputObjectFieldName)
  -- ^ duplicate field declaration in input objects
  | ObjectFieldTypeDoesNotExist
    !ObjectTypeName !ObjectFieldName !G.Name
  -- ^ field name and the field's base type
  | ObjectDuplicateFields !ObjectTypeName !(Set.HashSet G.Name)
  -- ^ duplicate field declaration in objects
  | ObjectFieldArgumentsNotAllowed !ObjectTypeName !ObjectFieldName
  -- ^ object fields can't have arguments
  | ObjectFieldObjectBaseType !ObjectTypeName !ObjectFieldName !G.Name
  -- ^ object fields can't have object types as base types
  | ObjectRelationshipTableDoesNotExist
    !ObjectTypeName !RelationshipName !QualifiedTable
  -- ^ The table specified in the relationship does not exist
  | ObjectRelationshipFieldDoesNotExist
    !ObjectTypeName !RelationshipName !ObjectFieldName
  -- ^ The field specified in the relationship mapping does not exist
  | ObjectRelationshipColumnDoesNotExist
    !ObjectTypeName !RelationshipName !QualifiedTable !PGCol
  -- ^ The column specified in the relationship mapping does not exist
  | DuplicateEnumValues !EnumTypeName !(Set.HashSet G.EnumValue)
  -- ^ duplicate enum values
  deriving (Show, Eq)

showCustomTypeValidationError
  :: CustomTypeValidationError -> T.Text
showCustomTypeValidationError = \case
  DuplicateTypeNames types ->
    "duplicate type names: " <> dquoteList types

  InputObjectFieldTypeDoesNotExist objType fieldName fieldTy ->
    "the type " <> fieldTy <<> " for field "
    <> fieldName <<> " in " <> " input object type "
    <> objType <<> " does not exist"

  InputObjectDuplicateFields objType fields ->
    "the input object " <> objType <<> " has duplicate fields: " <> dquoteList fields

  ObjectFieldTypeDoesNotExist objType fieldName fieldTy ->
    "the type " <> fieldTy <<> " for field "
    <> fieldName <<> " in " <> " object type "
    <> objType <<> " does not exist"

  ObjectDuplicateFields objType fields ->
    "the object " <> objType <<> " has duplicate fields: " <> dquoteList fields

  ObjectFieldArgumentsNotAllowed objType _ ->
    "the object " <> objType <<> " can't have arguments"

  ObjectFieldObjectBaseType objType fieldName fieldType ->
    "the type " <> fieldType <<> " of the field " <> fieldName
    <<> " in the object type " <> objType <<> " is object type which isn't allowed"

  ObjectRelationshipTableDoesNotExist objType relName table ->
    "the remote table " <> table <<> " for relationship " <> relName
    <<> " of object type " <> objType <<> " does not exist"

  ObjectRelationshipFieldDoesNotExist objType relName fieldName ->
    "the field " <> fieldName <<> " for relationship " <> relName
    <<> " in object type " <> objType <<> " does not exist"

  ObjectRelationshipColumnDoesNotExist objType relName remoteTable column ->
    "the column " <> column <<> " of remote table " <> remoteTable
    <<> " for relationship " <> relName <<> " of object type " <> objType
    <<> " does not exist"

  DuplicateEnumValues tyName values ->
    "the enum type " <> tyName <<> " has duplicate values: " <> dquoteList values


runSetCustomTypes
  :: ( MonadError QErr m
     , CacheRWM m
     , MonadTx m
     )
  => CustomTypes -> m EncJSON
runSetCustomTypes customTypes = do
  persistCustomTypes customTypes
  buildSchemaCacheFor MOCustomTypes
  return successMsg

persistCustomTypes :: MonadTx m => CustomTypes -> m ()
persistCustomTypes customTypes = liftTx do
  clearCustomTypes
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_custom_types
      (custom_types)
      VALUES ($1)
  |] (Identity $ Q.AltJ customTypes) False

clearCustomTypes :: Q.TxE QErr ()
clearCustomTypes = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.hdb_custom_types
  |] () False

resolveCustomTypes
  :: (MonadError QErr m)
  => TableCache -> CustomTypes -> m (NonObjectTypeMap, AnnotatedObjects)
resolveCustomTypes tableCache customTypes = do
  either (throw400 ConstraintViolation . showErrors) pure
    =<< runValidateT (validateCustomTypeDefinitions tableCache customTypes)
  _buildCustomTypesSchemaPartial tableCache customTypes
  where
    showErrors :: [CustomTypeValidationError] -> T.Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showCustomTypeValidationError) allErrors)
