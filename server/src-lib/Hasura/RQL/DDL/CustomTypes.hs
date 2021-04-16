module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes
  , clearCustomTypesInMetadata
  , resolveCustomTypes
  , lookupPGScalar
  ) where


import           Hasura.Prelude

import qualified Data.Dependent.Map                 as DMap
import qualified Data.HashMap.Strict                as Map
import qualified Data.HashSet                       as Set
import qualified Data.List.Extended                 as L
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Lens                       ((.~))
import           Control.Monad.Validate
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.SQL.Tag


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

-- | Validate the custom types and return any reused Postgres base types (as
-- scalars).
validateCustomTypeDefinitions
  :: forall m
   . MonadValidate [CustomTypeValidationError] m
  => SourceCache
  -> CustomTypes
  -> DMap.DMap BackendTag ScalarSet
  -- ^ all Postgres base types. See Note [Postgres scalars in custom types]
  -> m AnnotatedCustomTypes
validateCustomTypeDefinitions sources customTypes allScalars = do
  unless (null duplicateTypes) $ dispute $ pure $ DuplicateTypeNames duplicateTypes
  traverse_ validateEnum enumDefinitions
  reusedScalars <- execWriterT $ traverse_ validateInputObject inputObjectDefinitions
  annotatedObjects <- mapFromL (unObjectTypeName . _otdName . _aotDefinition) <$>
                      traverse validateObject objectDefinitions
  let scalarTypeMap = Map.map NOCTScalar $
        Map.map ASTCustom scalarTypes <> reusedScalars
      enumTypeMap = Map.map NOCTEnum enumTypes
      inputObjectTypeMap = Map.map NOCTInputObject inputObjectTypes
      nonObjectTypeMap = scalarTypeMap <> enumTypeMap <> inputObjectTypeMap
  pure $ AnnotatedCustomTypes nonObjectTypeMap annotatedObjects
  where
    sourceTables = Map.mapMaybe unsafeSourceTables sources
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
      mapFromL _stdName $ scalarDefinitions <> defaultScalars

    enumTypes =
      mapFromL (unEnumTypeName . _etdName) enumDefinitions

    inputObjectTypes =
      mapFromL (unInputObjectTypeName . _iotdName) inputObjectDefinitions

    validateEnum
      :: EnumTypeDefinition -> m ()
    validateEnum enumDefinition = do
      let duplicateEnumValues = L.duplicates $ map _evdValue $ toList $
                                _etdValues enumDefinition
      -- check for duplicate field names
      unless (null duplicateEnumValues) $
        dispute $ pure $ DuplicateEnumValues
        (_etdName enumDefinition) duplicateEnumValues

    validateInputObject
      :: InputObjectTypeDefinition -> WriterT (Map.HashMap G.Name AnnotatedScalarType) m ()
    validateInputObject inputObjectDefinition = do
      let inputObjectTypeName = _iotdName inputObjectDefinition
          duplicateFieldNames =
            L.duplicates $ map _iofdName $ toList $
            _iotdFields inputObjectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ InputObjectDuplicateFields
        inputObjectTypeName duplicateFieldNames

      let mapToSet = Set.fromList . Map.keys
          inputTypes =
            mapToSet scalarTypes `Set.union` mapToSet enumTypes `Set.union` mapToSet inputObjectTypes

      -- check that fields reference input types
      for_ (_iotdFields inputObjectDefinition) $ \inputObjectField -> do
        let fieldBaseType = G.getBaseType $ unGraphQLType $ _iofdType inputObjectField
        if | Set.member fieldBaseType inputTypes -> pure ()
           | Just scalarInfo <- lookupPGScalar allScalars fieldBaseType (ASTReusedScalar fieldBaseType) ->
               tell $ Map.singleton fieldBaseType scalarInfo
           | otherwise ->
               refute $ pure $ InputObjectFieldTypeDoesNotExist
                 (_iotdName inputObjectDefinition)
                 (_iofdName inputObjectField) fieldBaseType

    validateObject
      :: ObjectType -> m AnnotatedObjectType
    validateObject objectDefinition = do
      let objectTypeName = _otdName objectDefinition
          fieldNames = map (unObjectFieldName . _ofdName) $
                       toList (_otdFields objectDefinition)
          maybeRelationships = _otdRelationships objectDefinition
          relNames = maybe []
            (map (unRelationshipName . _trName) . toList) maybeRelationships
          duplicateFieldNames = L.duplicates $ fieldNames <> relNames
          fields = _otdFields objectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ ObjectDuplicateFields objectTypeName duplicateFieldNames

      scalarOrEnumFields <- for fields $ \objectField -> do
        let fieldName = _ofdName objectField
        -- check that arguments are not defined
        when (isJust $ _ofdArguments objectField) $
          dispute $ pure $ ObjectFieldArgumentsNotAllowed
          objectTypeName fieldName

        forM objectField $ \fieldType -> do
          let fieldBaseType = G.getBaseType $ unGraphQLType fieldType
              objectTypes = Set.fromList $ map (unObjectTypeName . _otdName)
                            objectDefinitions

          -- check that the fields only reference scalars and enums
          -- and not other object types
          annotatedObjectFieldType <-
            if | Just scalarDef <- Map.lookup fieldBaseType scalarTypes ->
                   pure $ AOFTScalar $ ASTCustom scalarDef
               | Just enumDef <- Map.lookup fieldBaseType enumTypes ->
                   pure $ AOFTEnum enumDef
               | Set.member fieldBaseType objectTypes ->
                   refute $ pure $ ObjectFieldObjectBaseType
                     objectTypeName fieldName fieldBaseType
               | Just scalarInfo <- lookupPGScalar allScalars fieldBaseType (AOFTScalar . ASTReusedScalar fieldBaseType) ->
                   pure scalarInfo
               | otherwise ->
                   refute $ pure $ ObjectFieldTypeDoesNotExist
                     objectTypeName fieldName fieldBaseType
          pure (unGraphQLType fieldType, annotatedObjectFieldType)

      let scalarOrEnumFieldMap = Map.fromList $
            map (_ofdName &&& (fst . _ofdType)) $ toList $ scalarOrEnumFields

      annotatedRelationships <- forM maybeRelationships $ \relationships -> do
        let headSource NE.:| rest = _trSource <$> relationships
        -- this check is needed to ensure that custom type relationships are all defined to a single source
        unless (all (headSource ==) rest) $
          refute $ pure $ ObjectRelationshipMultiSources objectTypeName
        forM relationships $ \TypeRelationship{..} -> do
          --check that the table exists
          remoteTableInfo <- onNothing (Map.lookup headSource sourceTables >>= Map.lookup _trRemoteTable) $
            refute $ pure $ ObjectRelationshipTableDoesNotExist
            objectTypeName _trName _trRemoteTable

          -- check that the column mapping is sane
          annotatedFieldMapping <- flip Map.traverseWithKey _trFieldMapping $
            \fieldName columnName -> do
              case Map.lookup fieldName scalarOrEnumFieldMap of
                Nothing -> dispute $ pure $ ObjectRelationshipFieldDoesNotExist
                           objectTypeName _trName fieldName
                Just fieldType ->
                  -- the field should be a non-list type scalar
                  when (G.isListType fieldType) $
                    dispute $ pure $ ObjectRelationshipFieldListType
                    objectTypeName _trName fieldName

              -- the column should be a column of the table
              onNothing (getColumnInfoM remoteTableInfo (fromCol @'Postgres columnName)) $ refute $ pure $
                ObjectRelationshipColumnDoesNotExist objectTypeName _trName _trRemoteTable columnName

          pure $ TypeRelationship _trName _trType _trSource remoteTableInfo annotatedFieldMapping

      let sourceConfig = do
            source     <- _trSource . NE.head <$> annotatedRelationships
            sourceInfo <- Map.lookup source sources
            (source,) <$> unsafeSourceConfiguration @'Postgres sourceInfo

      pure $ flip AnnotatedObjectType sourceConfig $
             ObjectTypeDefinition objectTypeName (_otdDescription objectDefinition)
             scalarOrEnumFields annotatedRelationships

-- see Note [Postgres scalars in custom types]
lookupPGScalar :: DMap.DMap BackendTag ScalarSet -> G.Name -> (PGScalarType -> a) -> Maybe a
lookupPGScalar allScalars baseType callback = afold $ do
  ScalarSet scalars <- DMap.lookup PostgresTag $ allScalars
  let scalarsMap = Map.fromList $ do
        scalar     <- Set.toList scalars
        scalarName <- afold $ scalarTypeGraphQLName scalar
        pure (scalarName, scalar)
  match <- afold $ Map.lookup baseType scalarsMap
  pure $ callback match

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
  | ObjectRelationshipFieldListType
    !ObjectTypeName !RelationshipName !ObjectFieldName
  -- ^ The field specified in the relationship mapping is a list type
  | ObjectRelationshipColumnDoesNotExist
    !ObjectTypeName !RelationshipName !QualifiedTable !PGCol
  -- ^ The column specified in the relationship mapping does not exist
  | ObjectRelationshipMultiSources !ObjectTypeName
  -- ^ Object relationship refers to table in multiple sources
  | DuplicateEnumValues !EnumTypeName !(Set.HashSet G.EnumValue)
  -- ^ duplicate enum values
  deriving (Show, Eq)

showCustomTypeValidationError
  :: CustomTypeValidationError -> Text
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

  ObjectRelationshipFieldListType objType relName fieldName ->
    "the type of the field " <> fieldName <<> " for relationship " <> relName
    <<> " in object type " <> objType <<> " is a list type"

  ObjectRelationshipColumnDoesNotExist objType relName remoteTable column ->
    "the column " <> column <<> " of remote table " <> remoteTable
    <<> " for relationship " <> relName <<> " of object type " <> objType
    <<> " does not exist"

  ObjectRelationshipMultiSources objType ->
    "the object " <> objType <<> " has relationships refers to tables in multiple sources"

  DuplicateEnumValues tyName values ->
    "the enum type " <> tyName <<> " has duplicate values: " <> dquoteList values


runSetCustomTypes
  :: ( MonadError QErr m
     , CacheRWM m
     , MetadataM m
     )
  => CustomTypes -> m EncJSON
runSetCustomTypes customTypes = do
  buildSchemaCacheFor MOCustomTypes $
    MetadataModifier $ metaCustomTypes .~ customTypes
  pure successMsg

clearCustomTypesInMetadata :: MetadataModifier
clearCustomTypesInMetadata =
  MetadataModifier $ metaCustomTypes .~ emptyCustomTypes

resolveCustomTypes
  :: MonadError QErr m
  => SourceCache
  -> CustomTypes
  -> DMap.DMap BackendTag ScalarSet
  -> m AnnotatedCustomTypes
resolveCustomTypes sources customTypes allScalars =
  either (throw400 ConstraintViolation . showErrors) pure
    =<< runValidateT (validateCustomTypeDefinitions sources customTypes allScalars)
  where
    showErrors :: [CustomTypeValidationError] -> Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showCustomTypeValidationError) allErrors)
