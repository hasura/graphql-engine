{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( validateRemoteRelationship
  , errorToText
  ) where

import           Hasura.Prelude                     hiding (first)

import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import qualified Language.GraphQL.Draft.Syntax      as G

import           Data.Foldable
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Utils               (getBaseTyWithNestedLevelsCount)
import           Hasura.RQL.Types
import           Hasura.SQL.Types


-- | An error validating the remote relationship.
data ValidationError
  = RemoteSchemaNotFound !RemoteSchemaName
  | CouldntFindRemoteField !G.Name !G.Name
  | FieldNotFoundInRemoteSchema !G.Name
  | NoSuchArgumentForRemote !G.Name
  | MissingRequiredArgument !G.Name
  | TypeNotFound !G.Name
  | TableNotFound !QualifiedTable
  | TableFieldNonexistent !QualifiedTable !FieldName
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType !Text
  | InvalidVariable !G.Name !(HM.HashMap G.Name (ColumnInfo 'Postgres))
  | NullNotAllowedHere
  | InvalidGTypeForStripping !G.GType
  | UnsupportedMultipleElementLists
  | UnsupportedEnum
  | InvalidGraphQLName !Text
  | IDTypeJoin !G.Name

errorToText :: ValidationError -> Text
errorToText = \case
  RemoteSchemaNotFound name ->
    "remote schema with name " <> name <<> " not found"
  CouldntFindRemoteField name ty ->
    "remote field with name " <> name <<> " and type " <> ty <<> " not found"
  FieldNotFoundInRemoteSchema name ->
    "field with name " <> name <<> " not found in remote schema"
  NoSuchArgumentForRemote name ->
    "argument with name " <> name <<> " not found in remote schema"
  MissingRequiredArgument name ->
    "required argument with name " <> name <<> " is missing"
  TypeNotFound ty ->
    "type with name " <> ty <<> " not found"
  TableNotFound name ->
    "table with name " <> name <<> " not found"
  TableFieldNonexistent table fieldName ->
    "field with name " <> fieldName <<> " not found in table " <>> table
  ExpectedTypeButGot expTy actualTy ->
    "expected type " <> G.getBaseType expTy <<> " but got " <>> G.getBaseType actualTy
  InvalidType ty err ->
    "type " <> G.getBaseType ty <<> err
  InvalidVariable var _ ->
    "variable " <> var <<> " is not found"
  NullNotAllowedHere ->
    "null is not allowed here"
  InvalidGTypeForStripping ty ->
    "type " <> G.getBaseType ty <<> " is invalid for stripping"
  UnsupportedMultipleElementLists ->
    "multiple elements in list value is not supported"
  UnsupportedEnum ->
    "enum value is not supported"
  InvalidGraphQLName t ->
    t <<> " is not a valid GraphQL identifier"
  IDTypeJoin typeName ->
    "Only ID, Int, uuid or String scalar types can be joined to the ID type, but recieved " <>> typeName

-- | Validate a remote relationship given a context.
validateRemoteRelationship
  :: (MonadError ValidationError m)
  => RemoteRelationship
  -> RemoteSchemaMap
  -> [ColumnInfo 'Postgres]
  -> m (RemoteFieldInfo 'Postgres)
validateRemoteRelationship remoteRelationship remoteSchemaMap pgColumns = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
      table = rtrTable remoteRelationship
  hasuraFields <- forM (toList $ rtrHasuraFields remoteRelationship) $
    \fieldName -> onNothing (find ((==) fieldName . fromPGCol . pgiColumn) pgColumns) $
      throwError $ TableFieldNonexistent table fieldName
  pgColumnsVariables <- mapM (\(k,v) -> do
                                  variableName <- pgColumnToVariable k
                                  pure $ (variableName,v)
                              ) $ HM.toList (mapFromL pgiColumn pgColumns)
  let pgColumnsVariablesMap = HM.fromList pgColumnsVariables
  (RemoteSchemaCtx rsName introspectionResult rsi _ _) <-
    onNothing (HM.lookup remoteSchemaName remoteSchemaMap) $
    throwError $ RemoteSchemaNotFound remoteSchemaName
  let schemaDoc@(G.SchemaIntrospection originalDefns) = irDoc introspectionResult
      queryRootName = irQueryRoot introspectionResult
  queryRoot <- onNothing (lookupObject schemaDoc queryRootName) $
    throwError $ FieldNotFoundInRemoteSchema queryRootName
  (_, (leafParamMap, leafTypeMap)) <-
    foldlM
    (buildRelationshipTypeInfo pgColumnsVariablesMap schemaDoc)
    (queryRoot,(mempty,mempty))
    (unRemoteFields $ rtrRemoteField remoteRelationship)
  pure $ RemoteFieldInfo
        { _rfiName = rtrName remoteRelationship
        , _rfiParamMap = leafParamMap
        , _rfiHasuraFields = HS.fromList hasuraFields
        , _rfiRemoteFields = rtrRemoteField remoteRelationship
        , _rfiRemoteSchema = rsi
        -- adding the new types after stripping the values to the
        -- schema document
        , _rfiSchemaIntrospect = G.SchemaIntrospection $ originalDefns <> HM.elems leafTypeMap
        , _rfiRemoteSchemaName = rsName
        }
  where
    getObjTyInfoFromField schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
      in lookupObject schemaDoc baseTy
    isValidType schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
      in
        case (lookupType schemaDoc baseTy) of
          Just (G.TypeDefinitionScalar _)    -> True
          Just (G.TypeDefinitionInterface _) -> True
          Just (G.TypeDefinitionUnion _)     -> True
          Just (G.TypeDefinitionEnum _)      -> True
          _                                  -> False
    buildRelationshipTypeInfo pgColumnsVariablesMap schemaDoc (objTyInfo,(_,typeMap)) fieldCall = do
      objFldDefinition <- lookupField (fcName fieldCall) objTyInfo
      let providedArguments = getRemoteArguments $ fcArguments fieldCall
      validateRemoteArguments
        (mapFromL G._ivdName (G._fldArgumentsDefinition objFldDefinition))
        providedArguments
        pgColumnsVariablesMap
        schemaDoc
      let eitherParamAndTypeMap =
            runStateT
              (stripInMap
                 remoteRelationship
                 schemaDoc
                 (mapFromL G._ivdName (G._fldArgumentsDefinition objFldDefinition))
                 providedArguments)
              $ typeMap
      (newParamMap, newTypeMap) <- onLeft eitherParamAndTypeMap $ throwError
      innerObjTyInfo <- onNothing (getObjTyInfoFromField schemaDoc objFldDefinition) $
        bool (throwError $
                    (InvalidType (G._fldType objFldDefinition) "only output type is expected"))
             (pure objTyInfo)
             (isValidType schemaDoc objFldDefinition)
      pure
       ( innerObjTyInfo
       , (newParamMap,newTypeMap))

-- | Return a map with keys deleted whose template argument is
-- specified as an atomic (variable, constant), keys which are kept
-- have their values modified by 'stripObject' or 'stripList'.
-- This function creates the 'HashMap G.Name G.InputValueDefinition' which modifies
-- the original input parameters (if any) of the remote node/table being used. Only
-- list or object types are preserved and other types are stripped off. The object or
-- list types are preserved because they can be merged, if any arguments are
-- provided by the user while querying a remote join field.
stripInMap
  :: RemoteRelationship
  -> G.SchemaIntrospection
  -> HM.HashMap G.Name G.InputValueDefinition
  -> HM.HashMap G.Name (G.Value G.Name)
  -> StateT
       (HashMap G.Name (G.TypeDefinition [G.Name]))
       (Either ValidationError)
       (HM.HashMap G.Name G.InputValueDefinition)
stripInMap remoteRelationship types schemaArguments providedArguments =
  fmap
    (HM.mapMaybe id)
    (HM.traverseWithKey
       (\name inpValInfo ->
          case HM.lookup name providedArguments of
            Nothing -> pure (Just inpValInfo)
            Just value -> do
              maybeNewGType <- stripValue remoteRelationship types (G._ivdType inpValInfo) value
              pure
                (fmap
                   (\newGType -> inpValInfo {G._ivdType = newGType})
                   maybeNewGType))
       schemaArguments)

-- | Strip a value type completely, or modify it, if the given value
-- is atomic-ish.
stripValue
  :: RemoteRelationship
  -> G.SchemaIntrospection
  -> G.GType
  -> G.Value G.Name
  -> StateT (HashMap G.Name (G.TypeDefinition [G.Name])) (Either ValidationError) (Maybe G.GType)
stripValue remoteRelationshipName types gtype value = do
  case value of
    G.VVariable {} -> pure Nothing
    G.VInt {} -> pure Nothing
    G.VFloat {} -> pure Nothing
    G.VString {} -> pure Nothing
    G.VBoolean {} -> pure Nothing
    G.VNull {} -> pure Nothing
    G.VEnum {} -> pure Nothing
    G.VList values ->
      case values of
        []       -> pure Nothing
        [gvalue] -> stripList remoteRelationshipName types gtype gvalue
        _        -> lift (Left UnsupportedMultipleElementLists)
    G.VObject keyPairs ->
      fmap Just (stripObject remoteRelationshipName types gtype keyPairs)

-- -- | Produce a new type for the list, or strip it entirely.
stripList
  :: RemoteRelationship
  -> G.SchemaIntrospection
  -> G.GType
  -> G.Value G.Name
  -> StateT (HashMap G.Name (G.TypeDefinition [G.Name])) (Either ValidationError) (Maybe G.GType)
stripList remoteRelationshipName types originalOuterGType value =
  case originalOuterGType of
    G.TypeList nullability innerGType -> do
      maybeNewInnerGType <- stripValue remoteRelationshipName types innerGType value
      pure (G.TypeList nullability <$> maybeNewInnerGType)
    _ -> lift (Left (InvalidGTypeForStripping originalOuterGType))

-- -- | Produce a new type for the given InpValInfo, modified by
-- -- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- -- object.
stripObject
  :: RemoteRelationship
  -> G.SchemaIntrospection
  -> G.GType
  -> HashMap G.Name (G.Value G.Name)
  -> StateT (HashMap G.Name (G.TypeDefinition [G.Name])) (Either ValidationError) G.GType
stripObject remoteRelationshipName schemaDoc originalGtype templateArguments =
  case originalGtype of
    G.TypeNamed nullability originalNamedType ->
      case lookupType schemaDoc (G.getBaseType originalGtype) of
        Just (G.TypeDefinitionInputObject originalInpObjTyInfo) -> do
          let originalSchemaArguments =
                mapFromL G._ivdName $ G._iotdValueDefinitions originalInpObjTyInfo
              newNamedType =
                renameNamedType
                  (renameTypeForRelationship remoteRelationshipName)
                  originalNamedType
          newSchemaArguments <-
            stripInMap
              remoteRelationshipName
              schemaDoc
              originalSchemaArguments
              templateArguments
          let newInpObjTyInfo =
                originalInpObjTyInfo
                  { G._iotdValueDefinitions = HM.elems newSchemaArguments
                  , G._iotdName = newNamedType
                  }
              newGtype = G.TypeNamed nullability newNamedType
          modify (HM.insert newNamedType (G.TypeDefinitionInputObject newInpObjTyInfo))
          pure newGtype
        _ -> lift (Left (InvalidGTypeForStripping originalGtype))
    _ -> lift (Left (InvalidGTypeForStripping originalGtype))

-- -- | Produce a new name for a type, used when stripping the schema
-- -- types for a remote relationship.
-- TODO: Consider a separator character to avoid conflicts. (from master)
renameTypeForRelationship :: RemoteRelationship -> Text -> Text
renameTypeForRelationship rtr text =
  text <> "_remote_rel_" <> name
  where name = schema <> "_" <> table <> remoteRelationshipNameToText (rtrName rtr)
        QualifiedObject (SchemaName schema) (TableName table) = rtrTable rtr

-- | Rename a type.
renameNamedType :: (Text -> Text) -> G.Name -> G.Name
renameNamedType rename =
  G.unsafeMkName . rename . G.unName

-- | Convert a field name to a variable name.
pgColumnToVariable :: MonadError ValidationError m => PGCol -> m G.Name
pgColumnToVariable pgCol =
  let pgColText = getPGColTxt pgCol
  in G.mkName pgColText `onNothing` throwError (InvalidGraphQLName pgColText)

-- | Lookup the field in the schema.
lookupField
  :: (MonadError ValidationError m)
  => G.Name
  -> G.ObjectTypeDefinition
  -> m G.FieldDefinition
lookupField name objFldInfo = viaObject objFldInfo
  where
    viaObject =
      maybe (throwError (CouldntFindRemoteField name $ G._otdName objFldInfo)) pure .
      lookup name .
      HM.toList .
      mapFromL G._fldName .
      G._otdFieldsDefinition

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments
  :: (MonadError ValidationError m)
  => HM.HashMap G.Name G.InputValueDefinition
  -> HM.HashMap G.Name (G.Value G.Name)
  -> HM.HashMap G.Name (ColumnInfo 'Postgres)
  -> G.SchemaIntrospection
  -> m ()
validateRemoteArguments expectedArguments providedArguments permittedVariables schemaDocument = do
  traverse_ validateProvided (HM.toList providedArguments)
  -- Not neccessary to validate if all required args are provided in the relationship
  -- traverse validateExpected (HM.toList expectedArguments)
  where
    validateProvided (providedName, providedValue) =
      case HM.lookup providedName expectedArguments of
        Nothing -> throwError (NoSuchArgumentForRemote providedName)
        Just (G._ivdType -> expectedType) ->
          validateType permittedVariables providedValue expectedType schemaDocument

unwrapGraphQLType :: G.GType -> G.GType
unwrapGraphQLType = \case
  G.TypeList _ lt -> lt
  nt              -> nt

-- | Validate a value against a type.
validateType
  :: (MonadError ValidationError m)
  => HM.HashMap G.Name (ColumnInfo 'Postgres)
  -> G.Value G.Name
  -> G.GType
  -> G.SchemaIntrospection
  -> m ()
validateType permittedVariables value expectedGType schemaDocument =
  case value of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> throwError (InvalidVariable variable permittedVariables)
        Just fieldInfo -> do
          namedType <- columnInfoToNamedType fieldInfo
          isTypeCoercible (mkGraphQLType namedType) expectedGType
    G.VInt {} -> do
      intScalarGType <- mkGraphQLType <$> getPGScalarTypeName PGInteger
      isTypeCoercible intScalarGType expectedGType
    G.VFloat {} -> do
      floatScalarGType <- mkGraphQLType <$> getPGScalarTypeName PGFloat
      isTypeCoercible floatScalarGType expectedGType
    G.VBoolean {} -> do
      boolScalarGType <- mkGraphQLType <$> getPGScalarTypeName PGBoolean
      isTypeCoercible boolScalarGType expectedGType
    G.VNull -> throwError NullNotAllowedHere
    G.VString {} -> do
      stringScalarGType <- mkGraphQLType <$> getPGScalarTypeName PGText
      isTypeCoercible stringScalarGType expectedGType
    G.VEnum _ -> throwError UnsupportedEnum
    G.VList values -> do
      case values of
        []  -> pure ()
        [_] -> pure ()
        _   -> throwError UnsupportedMultipleElementLists
      assertListType expectedGType
      for_
        values
        (\val ->
            validateType permittedVariables val (unwrapGraphQLType expectedGType) schemaDocument)
    G.VObject values ->
      for_
        (HM.toList values)
        (\(name,val) ->
           let expectedNamedType = G.getBaseType expectedGType
           in
           case lookupType schemaDocument expectedNamedType of
             Nothing -> throwError $ TypeNotFound expectedNamedType
             Just typeInfo ->
               case typeInfo of
                 G.TypeDefinitionInputObject inpObjTypeInfo ->
                   let objectTypeDefnsMap =
                         mapFromL G._ivdName $ G._iotdValueDefinitions inpObjTypeInfo
                   in
                   case HM.lookup name objectTypeDefnsMap of
                     Nothing -> throwError $ NoSuchArgumentForRemote name
                     Just (G._ivdType -> expectedType) ->
                       validateType permittedVariables val expectedType schemaDocument
                 _ -> do
                   throwError $ InvalidType (mkGraphQLType name) "not an input object type")
  where
    mkGraphQLType =
      G.TypeNamed (G.Nullability False)

isTypeCoercible
  :: (MonadError ValidationError m)
  => G.GType
  -> G.GType
  -> m ()
isTypeCoercible actualType expectedType =
  -- The GraphQL spec says that, a singleton type can be coerced into  an array
  -- type. Which means that if the 'actualType' is a singleton type, like
  -- 'Int' we should be able to join this with a remote node, which expects an
  -- input argument of type '[Int]'
  -- http://spec.graphql.org/June2018/#sec-Type-System.List
  let (actualBaseType, actualNestingLevel) = getBaseTyWithNestedLevelsCount actualType
      (expectedBaseType, expectedNestingLevel) = getBaseTyWithNestedLevelsCount expectedType
  in
  if | expectedBaseType == $$(G.litName "ID") ->
         bool (throwError $ IDTypeJoin actualBaseType)
              (pure ())
              -- Check under `Input Coercion` https://spec.graphql.org/June2018/#sec-ID
              -- We can also include the `ID` type in the below list but it will be
              -- extraneous because at the time of writing this, we don't generate
              -- the `ID` type in the DB schema
              (G.unName actualBaseType `elem`
               ["ID", "Int", "String", "bigint", "smallint" , "uuid"])
     | actualBaseType /= expectedBaseType -> raiseValidationError
       -- we cannot coerce two types with different nesting levels,
       -- for example, we cannot coerce [Int] to [[Int]]
     | (actualNestingLevel == expectedNestingLevel || actualNestingLevel == 0) -> pure ()
     | otherwise -> raiseValidationError
     where
       raiseValidationError = throwError $ ExpectedTypeButGot expectedType actualType

getPGScalarTypeName :: MonadError ValidationError m => PGScalarType -> m G.Name
getPGScalarTypeName scalarType =
  runExceptT (mkScalarTypeName scalarType) >>=
    flip onLeft (\ _ -> throwError $ InvalidGraphQLName $ toSQLTxt scalarType)

assertListType :: (MonadError ValidationError m) => G.GType -> m ()
assertListType actualType =
  unless (G.isListType actualType)
    (throwError $ InvalidType actualType "is not a list type")

-- | Convert a field info to a named type, if possible.
columnInfoToNamedType
  :: (MonadError ValidationError m)
  => ColumnInfo 'Postgres
  -> m G.Name
columnInfoToNamedType pci =
  case pgiType pci of
    ColumnScalar scalarType -> getPGScalarTypeName scalarType
    _                       -> throwError UnsupportedEnum
