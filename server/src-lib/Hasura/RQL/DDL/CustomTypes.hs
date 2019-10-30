module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes
  , runSetCustomTypes_
  , clearCustomTypes
  , validateCustomTypesAndAddToCache
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

import           Hasura.GraphQL.Schema.CustomTypes (buildCustomTypesSchemaPartial)

validateCustomTypeDefinitions
  :: (MonadValidate [CustomTypeValidationError] m, CacheRM m)
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

    scalarTypes =
      Set.fromList $ map _stdName scalarDefinitions <> defaultScalars

    enumTypes =
      Set.fromList $ map (unEnumTypeName . _etdName) enumDefinitions

    -- TODO, clean it up maybe?
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
      :: (MonadValidate [CustomTypeValidationError] m, CacheRM m)
      => ObjectTypeDefinition -> m ()
    validateObject objectDefinition = do
      let objectTypeName = _otdName objectDefinition
          fieldNames = map (unObjectFieldName . _ofdName) $
                       toList (_otdFields objectDefinition)
          relationships = fromMaybe [] $ _otdRelationships objectDefinition
          relNames = map (unObjectRelationshipName . _orName) relationships
          duplicateFieldNames = L.duplicates $ fieldNames <> relNames
          fields = toList $ _otdFields objectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ ObjectDuplicateFields objectTypeName duplicateFieldNames

      scalarFields <- fmap (Map.fromList . catMaybes) $
        for fields $ \objectField -> do
        let fieldType = unGraphQLType $ _ofdType objectField
            fieldBaseType = G.getBaseType fieldType
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
        let relationshipName = _orName relationshipField
            remoteTable = _orRemoteTable relationshipField
            fieldMapping = _orFieldMapping relationshipField

        --check that the table exists
        remoteTableInfoM <- askTabInfoM remoteTable
        remoteTableInfo <- onNothing remoteTableInfoM $
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

isListType :: G.GType -> Bool
isListType = \case
  G.TypeList _ _  -> True
  G.TypeNamed _ _ -> False

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
  | ObjectDuplicateFields !ObjectTypeName !(Set.HashSet G.Name)
  -- ^ object fields can't have arguments
  | ObjectFieldArgumentsNotAllowed !ObjectTypeName !ObjectFieldName
  -- ^ object fields can't have object types as base types
  | ObjectFieldObjectBaseType !ObjectTypeName !ObjectFieldName !G.NamedType
  -- ^ The table specified in the relationship does not exist
  | ObjectRelationshipTableDoesNotExist
    !ObjectTypeName !ObjectRelationshipName !QualifiedTable
  -- ^ The field specified in the relationship mapping does not exist
  | ObjectRelationshipFieldDoesNotExist
    !ObjectTypeName !ObjectRelationshipName !ObjectFieldName
  -- ^ The column specified in the relationship mapping does not exist
  | ObjectRelationshipColumnDoesNotExist
    !ObjectTypeName !ObjectRelationshipName !QualifiedTable !PGCol
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
    INSERT into hdb_catalog.hdb_custom_types
      (custom_types)
      VALUES ($1)
  |] (Identity $ Q.AltJ customTypes) False

clearCustomTypes :: Q.TxE QErr ()
clearCustomTypes = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.hdb_custom_types
  |] () False

validateCustomTypesAndAddToCache
  :: ( MonadError QErr m
     , CacheRWM m
     )
  => CustomTypes -> m ()
validateCustomTypesAndAddToCache customTypes = do
  schemaCache <- askSchemaCache
  either (throw400 ConstraintViolation . showErrors) pure
    =<< runValidateT ( flip runReaderT schemaCache $
                       validateCustomTypeDefinitions customTypes)
  buildCustomTypesSchemaPartial customTypes >>= setCustomTypesInCache
  where
    showErrors :: [CustomTypeValidationError] -> T.Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showCustomTypeValidationError) allErrors)
