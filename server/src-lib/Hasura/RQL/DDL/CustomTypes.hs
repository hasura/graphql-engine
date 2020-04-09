module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes
  , persistCustomTypes
  , clearCustomTypes
  , resolveCustomTypes
  ) where

import           Control.Monad.Validate

import qualified Control.Monad.Writer.Strict       as Writer
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as Set
import qualified Data.List.Extended                as L
import qualified Data.Text                         as T
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Validate.Types     (mkScalarTy)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Hasura.GraphQL.Schema.CustomTypes (buildCustomTypesSchemaPartial)

{- Note [Postgres scalars in custom types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It’s very convenient to be able to reference Postgres scalars in custom type
definitions. For example, we might have a type like this:

    type User {
      id: uuid!
      name: String!
      location: geography
    }

The uuid and geography types are Postgres scalars, not separately-defined
GraphQL types. To support this, we have to take a few extra steps:

  1. The set of Postgres base types is not fixed; extensions like PostGIS add
     new ones, and users can even define their own. Therefore, we fetch the
     currently defined base types from the @pg_catalog.pg_type@ system table as part of
     loading the metadata.

  2. It’s possible for a custom type definition to use a type that doesn’t
     appear elsewhere in the GraphQL schema, so we record which base types were
     referenced while validating the custom type definitions and make sure to
     include them in the generated schema explicitly.
-}

-- | Validate the custom types and return any reused Postgres base types (as scalars)
validateCustomTypeDefinitions
  :: (MonadValidate [CustomTypeValidationError] m)
  => TableCache
  -> CustomTypes
  -> [PGScalarType] -- ^ List of all Postgres base types
  -> m (Set.HashSet PGScalarType) -- ^ Reused Postgres base types (as scalars) in custom types
validateCustomTypeDefinitions tableCache customTypes allPGScalars = do
  unless (null duplicateTypes) $ dispute $ pure $ DuplicateTypeNames duplicateTypes
  traverse_ validateEnum enumDefinitions
  -- The following validations also accumulates reused Postgres scalar types
  -- in a 'WriterT' monad. Using 'execWriterT' to peel of the monad and collect
  -- Postgres scalar types.
  Writer.execWriterT $ do
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

    defaultScalars = map G.NamedType ["Int", "Float", "String", "Boolean", "ID"]

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
      :: ( MonadValidate [CustomTypeValidationError] m
         , MonadWriter (Set.HashSet PGScalarType) m
         )
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
        if Set.member fieldBaseType inputTypes then pure ()
        else
          -- Check if field type uses any Postgres scalar type and return the same
          case findReusedPGScalar fieldBaseType of
            Just reusedScalar -> Writer.tell $ Set.singleton reusedScalar
            Nothing ->
              refute $ pure $ InputObjectFieldTypeDoesNotExist
                (_iotdName inputObjectDefinition)
                (_iofdName inputObjectField) fieldBaseType

    validateObject
      :: ( MonadValidate [CustomTypeValidationError] m
         , MonadWriter (Set.HashSet PGScalarType) m
         )
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
        if | Set.member fieldBaseType scalarTypes -> pure ()
           | Set.member fieldBaseType enumTypes -> pure ()
           | Set.member fieldBaseType objectTypes ->
               dispute $ pure $ ObjectFieldObjectBaseType
                 objectTypeName fieldName fieldBaseType
           | otherwise ->
               -- Check if field type uses any Postgres scalar type and return the same
               case findReusedPGScalar fieldBaseType of
                 Just ty -> Writer.tell $ Set.singleton ty
                 Nothing ->
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

    findReusedPGScalar baseType =
      find ((==) baseType . mkScalarTy) allPGScalars

data CustomTypeValidationError
  = DuplicateTypeNames !(Set.HashSet G.NamedType)
  -- ^ type names have to be unique across all types
  | InputObjectFieldTypeDoesNotExist
    !InputObjectTypeName !InputObjectFieldName !G.NamedType
  -- ^ field name and the field's base type
  | InputObjectDuplicateFields
    !InputObjectTypeName !(Set.HashSet InputObjectFieldName)
  -- ^ duplicate field declaration in input objects
  | ObjectFieldTypeDoesNotExist
    !ObjectTypeName !ObjectFieldName !G.NamedType
  -- ^ field name and the field's base type
  | ObjectDuplicateFields !ObjectTypeName !(Set.HashSet G.Name)
  -- ^ duplicate field declaration in objects
  | ObjectFieldArgumentsNotAllowed !ObjectTypeName !ObjectFieldName
  -- ^ object fields can't have arguments
  | ObjectFieldObjectBaseType !ObjectTypeName !ObjectFieldName !G.NamedType
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
  => TableCache -> CustomTypes -> [PGScalarType] -> m (NonObjectTypeMap, AnnotatedObjects)
resolveCustomTypes tableCache customTypes allPGScalars = do
  reusedPGScalars <- either (throw400 ConstraintViolation . showErrors) pure
    =<< runValidateT (validateCustomTypeDefinitions tableCache customTypes allPGScalars)
  buildCustomTypesSchemaPartial tableCache customTypes $ toList reusedPGScalars
  where
    showErrors :: [CustomTypeValidationError] -> T.Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showCustomTypeValidationError) allErrors)
