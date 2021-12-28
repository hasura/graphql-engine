module Hasura.RQL.DDL.CustomTypes
  ( runSetCustomTypes,
    clearCustomTypesInMetadata,
    resolveCustomTypes,
    lookupPGScalar,
  )
where

import Control.Lens ((.~))
import Control.Monad.Validate
import Data.Dependent.Map qualified as DMap
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.Extended
import Data.List.Extended qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.SQL.Tag
import Language.GraphQL.Draft.Syntax qualified as G

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
validateCustomTypeDefinitions ::
  forall m.
  MonadValidate [CustomTypeValidationError] m =>
  SourceCache ->
  CustomTypes ->
  -- | all Postgres base types. See Note [Postgres scalars in custom types]
  DMap.DMap BackendTag ScalarSet ->
  m AnnotatedCustomTypes
validateCustomTypeDefinitions sources customTypes allScalars = do
  unless (null duplicateTypes) $ dispute $ pure $ DuplicateTypeNames duplicateTypes
  traverse_ validateEnum enumDefinitions
  reusedScalars <- execWriterT $ traverse_ validateInputObject inputObjectDefinitions
  annotatedObjects <-
    mapFromL (unObjectTypeName . _otdName . _aotDefinition)
      <$> traverse validateObject objectDefinitions
  let scalarTypeMap =
        Map.map NOCTScalar $
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
      map _stdName scalarDefinitions
        <> map (unEnumTypeName . _etdName) enumDefinitions
        <> map (unInputObjectTypeName . _iotdName) inputObjectDefinitions
        <> map (unObjectTypeName . _otdName) objectDefinitions

    scalarTypes =
      mapFromL _stdName $ scalarDefinitions <> defaultScalars

    enumTypes =
      mapFromL (unEnumTypeName . _etdName) enumDefinitions

    objectTypes =
      mapFromL (unObjectTypeName . _otdName) objectDefinitions

    inputObjectTypes =
      mapFromL (unInputObjectTypeName . _iotdName) inputObjectDefinitions

    validateEnum ::
      EnumTypeDefinition -> m ()
    validateEnum enumDefinition = do
      let duplicateEnumValues =
            L.duplicates $
              map _evdValue $
                toList $
                  _etdValues enumDefinition
      -- check for duplicate field names
      unless (null duplicateEnumValues) $
        dispute $
          pure $
            DuplicateEnumValues
              (_etdName enumDefinition)
              duplicateEnumValues

    validateInputObject ::
      InputObjectTypeDefinition -> WriterT (Map.HashMap G.Name AnnotatedScalarType) m ()
    validateInputObject inputObjectDefinition = do
      let inputObjectTypeName = _iotdName inputObjectDefinition
          duplicateFieldNames =
            L.duplicates $
              map _iofdName $
                toList $
                  _iotdFields inputObjectDefinition

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $
          pure $
            InputObjectDuplicateFields
              inputObjectTypeName
              duplicateFieldNames

      let mapToSet = Set.fromList . Map.keys
          inputTypes =
            mapToSet scalarTypes `Set.union` mapToSet enumTypes `Set.union` mapToSet inputObjectTypes

      -- check that fields reference input types
      for_ (_iotdFields inputObjectDefinition) $ \inputObjectField -> do
        let fieldBaseType = G.getBaseType $ unGraphQLType $ _iofdType inputObjectField
        if
            | Set.member fieldBaseType inputTypes -> pure ()
            | Just scalarInfo <- lookupPGScalar allScalars fieldBaseType (ASTReusedScalar fieldBaseType) ->
              tell $ Map.singleton fieldBaseType scalarInfo
            | otherwise ->
              refute $
                pure $
                  InputObjectFieldTypeDoesNotExist
                    (_iotdName inputObjectDefinition)
                    (_iofdName inputObjectField)
                    fieldBaseType

    validateObject ::
      ObjectType -> m AnnotatedObjectType
    validateObject ObjectTypeDefinition {..} = do
      let fieldNames =
            map (unObjectFieldName . _ofdName) $
              toList _otdFields
          relNames =
            maybe
              []
              (map (unRelationshipName . _trName) . toList)
              _otdRelationships
          duplicateFieldNames = L.duplicates $ fieldNames <> relNames

      -- check for duplicate field names
      unless (null duplicateFieldNames) $
        dispute $ pure $ ObjectDuplicateFields _otdName duplicateFieldNames

      fields <- for _otdFields $ \objectField -> do
        let fieldName = _ofdName objectField
        -- check that arguments are not defined
        when (isJust $ _ofdArguments objectField) $
          dispute $
            pure $
              ObjectFieldArgumentsNotAllowed
                _otdName
                fieldName

        forM objectField $ \fieldType -> do
          let fieldBaseType = G.getBaseType $ unGraphQLType fieldType
          annotatedObjectFieldType <-
            if
                | Just scalarDef <- Map.lookup fieldBaseType scalarTypes ->
                  pure $ AOFTScalar $ ASTCustom scalarDef
                | Just enumDef <- Map.lookup fieldBaseType enumTypes ->
                  pure $ AOFTEnum enumDef
                | Map.member fieldBaseType objectTypes ->
                  pure $ AOFTObject fieldBaseType
                | Just scalarInfo <- lookupPGScalar allScalars fieldBaseType (AOFTScalar . ASTReusedScalar fieldBaseType) ->
                  pure scalarInfo
                | otherwise ->
                  refute $
                    pure $
                      ObjectFieldTypeDoesNotExist
                        _otdName
                        fieldName
                        fieldBaseType
          pure (unGraphQLType fieldType, annotatedObjectFieldType)

      let fieldsMap =
            Map.fromList $
              map (_ofdName &&& (fst . _ofdType)) $ toList fields

      annotatedRelationships <- forM _otdRelationships $ \relationships -> do
        let headSource NE.:| rest = _trSource <$> relationships
        -- this check is needed to ensure that custom type relationships are all defined to a single source
        unless (all (headSource ==) rest) $
          refute $ pure $ ObjectRelationshipMultiSources _otdName
        forM relationships $ \TypeRelationship {..} -> do
          --check that the table exists
          remoteTableInfo <-
            onNothing (Map.lookup headSource sourceTables >>= Map.lookup _trRemoteTable) $
              refute $
                pure $
                  ObjectRelationshipTableDoesNotExist
                    _otdName
                    _trName
                    _trRemoteTable

          -- check that the column mapping is sane
          annotatedFieldMapping <- flip Map.traverseWithKey _trFieldMapping $
            \fieldName columnName -> do
              case Map.lookup fieldName fieldsMap of
                Nothing ->
                  dispute $
                    pure $
                      ObjectRelationshipFieldDoesNotExist
                        _otdName
                        _trName
                        fieldName
                Just fieldType ->
                  -- the field should be a non-list type scalar
                  when (G.isListType fieldType) $
                    dispute $
                      pure $
                        ObjectRelationshipFieldListType
                          _otdName
                          _trName
                          fieldName

              -- the column should be a column of the table
              onNothing (getColumnInfoM remoteTableInfo (fromCol @('Postgres 'Vanilla) columnName)) $
                refute $
                  pure $
                    ObjectRelationshipColumnDoesNotExist _otdName _trName _trRemoteTable columnName

          pure $ TypeRelationship _trName _trType _trSource remoteTableInfo annotatedFieldMapping

      let sourceConfig = do
            source <- _trSource . NE.head <$> annotatedRelationships
            sourceInfo <- Map.lookup source sources
            (source,) <$> unsafeSourceConfiguration @('Postgres 'Vanilla) sourceInfo

      pure $
        flip AnnotatedObjectType sourceConfig $
          ObjectTypeDefinition
            _otdName
            _otdDescription
            fields
            annotatedRelationships

-- see Note [Postgres scalars in custom types]
lookupPGScalar ::
  DMap.DMap BackendTag ScalarSet ->
  G.Name ->
  (PGScalarType -> a) ->
  Maybe a
lookupPGScalar allScalars baseType callback = afold $ do
  ScalarSet scalars <- DMap.lookup (backendTag @('Postgres 'Vanilla)) $ allScalars
  let scalarsMap = Map.fromList $ do
        scalar <- Set.toList scalars
        scalarName <- afold $ scalarTypeGraphQLName @('Postgres 'Vanilla) scalar
        pure (scalarName, scalar)
  match <- afold $ Map.lookup baseType scalarsMap
  pure $ callback match

data CustomTypeValidationError
  = -- | type names have to be unique across all types
    DuplicateTypeNames !(Set.HashSet G.Name)
  | -- | field name and the field's base type
    InputObjectFieldTypeDoesNotExist
      !InputObjectTypeName
      !InputObjectFieldName
      !G.Name
  | -- | duplicate field declaration in input objects
    InputObjectDuplicateFields
      !InputObjectTypeName
      !(Set.HashSet InputObjectFieldName)
  | -- | field name and the field's base type
    ObjectFieldTypeDoesNotExist
      !ObjectTypeName
      !ObjectFieldName
      !G.Name
  | -- | duplicate field declaration in objects
    ObjectDuplicateFields !ObjectTypeName !(Set.HashSet G.Name)
  | -- | object fields can't have arguments
    ObjectFieldArgumentsNotAllowed !ObjectTypeName !ObjectFieldName
  | -- | object fields can't have object types as base types
    ObjectFieldObjectBaseType !ObjectTypeName !ObjectFieldName !G.Name
  | -- | The table specified in the relationship does not exist
    ObjectRelationshipTableDoesNotExist
      !ObjectTypeName
      !RelationshipName
      !QualifiedTable
  | -- | The field specified in the relationship mapping does not exist
    ObjectRelationshipFieldDoesNotExist
      !ObjectTypeName
      !RelationshipName
      !ObjectFieldName
  | -- | The field specified in the relationship mapping is a list type
    ObjectRelationshipFieldListType
      !ObjectTypeName
      !RelationshipName
      !ObjectFieldName
  | -- | The column specified in the relationship mapping does not exist
    ObjectRelationshipColumnDoesNotExist
      !ObjectTypeName
      !RelationshipName
      !QualifiedTable
      !PGCol
  | -- | Object relationship refers to table in multiple sources
    ObjectRelationshipMultiSources !ObjectTypeName
  | -- | duplicate enum values
    DuplicateEnumValues !EnumTypeName !(Set.HashSet G.EnumValue)
  deriving (Show, Eq)

showCustomTypeValidationError ::
  CustomTypeValidationError -> Text
showCustomTypeValidationError = \case
  DuplicateTypeNames types ->
    "duplicate type names: " <> dquoteList types
  InputObjectFieldTypeDoesNotExist objType fieldName fieldTy ->
    "the type " <> fieldTy <<> " for field "
      <> fieldName <<> " in "
      <> " input object type "
      <> objType <<> " does not exist"
  InputObjectDuplicateFields objType fields ->
    "the input object " <> objType <<> " has duplicate fields: " <> dquoteList fields
  ObjectFieldTypeDoesNotExist objType fieldName fieldTy ->
    "the type " <> fieldTy <<> " for field "
      <> fieldName <<> " in "
      <> " object type "
      <> objType <<> " does not exist"
  ObjectDuplicateFields objType fields ->
    "the object " <> objType <<> " has duplicate fields: " <> dquoteList fields
  ObjectFieldArgumentsNotAllowed objType _ ->
    "the object " <> objType <<> " can't have arguments"
  ObjectFieldObjectBaseType objType fieldName fieldType ->
    "the type " <> fieldType <<> " of the field " <> fieldName
      <<> " in the object type " <> objType
      <<> " is object type which isn't allowed"
  ObjectRelationshipTableDoesNotExist objType relName table ->
    "the remote table " <> table <<> " for relationship " <> relName
      <<> " of object type " <> objType
      <<> " does not exist"
  ObjectRelationshipFieldDoesNotExist objType relName fieldName ->
    "the field " <> fieldName <<> " for relationship " <> relName
      <<> " in object type " <> objType
      <<> " does not exist"
  ObjectRelationshipFieldListType objType relName fieldName ->
    "the type of the field " <> fieldName <<> " for relationship " <> relName
      <<> " in object type " <> objType
      <<> " is a list type"
  ObjectRelationshipColumnDoesNotExist objType relName remoteTable column ->
    "the column " <> column <<> " of remote table " <> remoteTable
      <<> " for relationship " <> relName
      <<> " of object type " <> objType
      <<> " does not exist"
  ObjectRelationshipMultiSources objType ->
    "the object " <> objType <<> " has relationships refers to tables in multiple sources"
  DuplicateEnumValues tyName values ->
    "the enum type " <> tyName <<> " has duplicate values: " <> dquoteList values

runSetCustomTypes ::
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  CustomTypes ->
  m EncJSON
runSetCustomTypes customTypes = do
  buildSchemaCacheFor MOCustomTypes $
    MetadataModifier $ metaCustomTypes .~ customTypes
  pure successMsg

clearCustomTypesInMetadata :: MetadataModifier
clearCustomTypesInMetadata =
  MetadataModifier $ metaCustomTypes .~ emptyCustomTypes

resolveCustomTypes ::
  MonadError QErr m =>
  SourceCache ->
  CustomTypes ->
  DMap.DMap BackendTag ScalarSet ->
  m AnnotatedCustomTypes
resolveCustomTypes sources customTypes allScalars =
  runValidate (validateCustomTypeDefinitions sources customTypes allScalars)
    `onLeft` (throw400 ConstraintViolation . showErrors)
  where
    showErrors :: [CustomTypeValidationError] -> Text
    showErrors allErrors =
      "validation for the given custom types failed " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showCustomTypeValidationError singleError
          _ ->
            "for the following reasons:\n"
              <> T.unlines
                (map (("  • " <>) . showCustomTypeValidationError) allErrors)
