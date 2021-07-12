{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( validateRemoteRelationship
  , errorToText
  ) where

import           Hasura.Prelude                      hiding (first)

import qualified Data.HashMap.Strict                 as HM
import qualified Data.HashSet                        as HS
import qualified Language.GraphQL.Draft.Syntax       as G

import           Data.Text.Extended

import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Backend


-- | An error validating the remote relationship.
data ValidationError (b :: BackendType)
  = RemoteSchemaNotFound !RemoteSchemaName
  | CouldntFindRemoteField !G.Name !G.Name
  | FieldNotFoundInRemoteSchema !G.Name
  | NoSuchArgumentForRemote !G.Name
  | MissingRequiredArgument !G.Name
  | TypeNotFound !G.Name
  | TableNotFound !(TableName b)
  | TableFieldNonexistent !(TableName b) !FieldName
  | TableFieldNotSupported !FieldName
  | TableComputedFieldWithInputArgs !FieldName !(FunctionName b)
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType !Text
  | InvalidVariable !G.Name !(HM.HashMap G.Name (DBJoinField b))
  | NullNotAllowedHere
  | InvalidGTypeForStripping !G.GType
  | UnsupportedMultipleElementLists
  | UnsupportedEnum
  | UnsupportedTableComputedField !(TableName b) !ComputedFieldName
  | InvalidGraphQLName !Text
  | IDTypeJoin !G.Name
  -- | This is the case where the type of the columns that are mapped do not
  -- have a graphql representation. This case is probably not reachable as
  -- having a db type which can't be representable in GraphQL should definitely
  -- fail the entire schema generation process
  | CannotGenerateGraphQLTypeName !(ScalarType b)

deriving instance Backend b => Eq (ValidationError b)


errorToText :: Backend b => ValidationError b -> Text
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
  TableFieldNotSupported fieldName ->
    "field with name " <> fieldName <<> " not supported; only columns and scalar computed fields"
  TableComputedFieldWithInputArgs fieldName function ->
    "computed field " <> fieldName <<> " is associated with SQL function " <> function
    <<> " has input arguments other than table row type and hasura session"
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
  UnsupportedTableComputedField tableName fieldName ->
    "computed field " <> fieldName <<> " returns set of " <> tableName <<> ", is not supported"
  InvalidGraphQLName t ->
    t <<> " is not a valid GraphQL identifier"
  IDTypeJoin typeName ->
    "Only ID, Int, uuid or String scalar types can be joined to the ID type, but received " <>> typeName
  CannotGenerateGraphQLTypeName typeName ->
    "the name of the scalar type " <> toTxt typeName <>
    " is not a valid GraphQL identifier, " <>
    " so columns of such type cannot be used in a remote schema mapping "

-- | Validate a remote relationship given a context.
validateRemoteRelationship
  :: forall b m
  .  (Backend b, MonadError (ValidationError b) m)
  => RemoteRelationship b
  -> (RemoteSchemaInfo, IntrospectionResult)
  -> FieldInfoMap (FieldInfo b)
  -> m (RemoteFieldInfo b)
validateRemoteRelationship remoteRelationship (remoteSchemaInfo, introspectionResult) fields = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
      table = rtrTable remoteRelationship
  hasuraFields <- forM (toList $ rtrHasuraFields remoteRelationship) $ \fieldName -> do
    fieldInfo <- onNothing (HM.lookup fieldName fields) $ throwError $ TableFieldNonexistent table fieldName
    case fieldInfo of
      FIColumn columnInfo               -> pure $ JoinColumn columnInfo
      FIComputedField ComputedFieldInfo{..} -> do
        scalarType <- case _cfiReturnType of
          CFRScalar ty    -> pure ty
          CFRSetofTable{} -> throwError $ UnsupportedTableComputedField table _cfiName
        let ComputedFieldFunction{..} = _cfiFunction
        case toList _cffInputArgs of
          [] -> pure $ JoinComputedField $ ScalarComputedField _cfiXComputedFieldInfo _cfiName _cffName
                                           _cffTableArgument _cffSessionArgument scalarType
          _ -> throwError $ TableComputedFieldWithInputArgs fieldName _cffName
      _                                 -> throwError $ TableFieldNotSupported fieldName
  hasuraFieldsVariablesMap <-
    fmap HM.fromList $ for hasuraFields $ \field -> (, field) <$> hasuraFieldToVariable field
  let schemaDoc     = irDoc introspectionResult
      queryRootName = irQueryRoot introspectionResult
  queryRoot <- onNothing (lookupObject schemaDoc queryRootName) $
    throwError $ FieldNotFoundInRemoteSchema queryRootName
  (_, (leafParamMap, leafTypeMap)) <-
    foldlM
    (buildRelationshipTypeInfo hasuraFieldsVariablesMap schemaDoc)
    (queryRoot, (mempty, mempty))
    (unRemoteFields $ rtrRemoteField remoteRelationship)
  pure $ RemoteFieldInfo
        { _rfiName = rtrName remoteRelationship
        , _rfiParamMap = leafParamMap
        , _rfiHasuraFields = HS.fromList hasuraFields
        , _rfiRemoteFields = rtrRemoteField remoteRelationship
        , _rfiRemoteSchema = remoteSchemaInfo
        -- adding the new input types after stripping the values of the
        -- schema document
        , _rfiInputValueDefinitions = HM.elems leafTypeMap
        , _rfiRemoteSchemaName = remoteSchemaName
        , _rfiTable = (table, rtrSource remoteRelationship)
        }
  where
    getObjTyInfoFromField
      :: RemoteSchemaIntrospection
      -> G.FieldDefinition RemoteSchemaInputValueDefinition
      -> Maybe (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
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

    buildRelationshipTypeInfo
      :: HashMap G.Name (DBJoinField b)
      -> RemoteSchemaIntrospection
      -> (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition,
           ( (HashMap G.Name RemoteSchemaInputValueDefinition)
           , (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))))
      -> FieldCall
      -> m ( G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
           , ( HashMap G.Name RemoteSchemaInputValueDefinition
             , HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)))
    buildRelationshipTypeInfo hasuraFieldsVariablesMap schemaDoc (objTyInfo,(_,typeMap)) fieldCall = do
      objFldDefinition <- lookupField (fcName fieldCall) objTyInfo
      let providedArguments = getRemoteArguments $ fcArguments fieldCall
      (validateRemoteArguments
        (mapFromL (G._ivdName . _rsitdDefinition) (G._fldArgumentsDefinition objFldDefinition))
        providedArguments
        hasuraFieldsVariablesMap
        schemaDoc)
      let eitherParamAndTypeMap =
            runStateT
              (stripInMap
                 remoteRelationship
                 schemaDoc
                 (mapFromL (G._ivdName . _rsitdDefinition) (G._fldArgumentsDefinition objFldDefinition))
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
       , (newParamMap, newTypeMap))

-- | Return a map with keys deleted whose template argument is
-- specified as an atomic (variable, constant), keys which are kept
-- have their values modified by 'stripObject' or 'stripList'.
-- This function creates the 'HashMap G.Name G.InputValueDefinition' which modifies
-- the original input parameters (if any) of the remote node/table being used. Only
-- list or object types are preserved and other types are stripped off. The object or
-- list types are preserved because they can be merged, if any arguments are
-- provided by the user while querying a remote join field.
stripInMap
  :: (Backend b)
  => RemoteRelationship b
  -> RemoteSchemaIntrospection
  -> HM.HashMap G.Name RemoteSchemaInputValueDefinition
  -> HM.HashMap G.Name (G.Value G.Name)
  -> StateT
       (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
       (Either (ValidationError b))
       (HM.HashMap G.Name RemoteSchemaInputValueDefinition)
stripInMap remoteRelationship types schemaArguments providedArguments =
  fmap
    (HM.mapMaybe id)
    (HM.traverseWithKey
       (\name remoteInpValDef@(RemoteSchemaInputValueDefinition inpValInfo _preset) ->
          case HM.lookup name providedArguments of
            Nothing -> pure $ Just remoteInpValDef
            Just value -> do
              maybeNewGType <- stripValue remoteRelationship types (G._ivdType inpValInfo) value
              pure
                (fmap
                   (\newGType ->
                      let newInpValInfo = inpValInfo {G._ivdType = newGType}
                      in RemoteSchemaInputValueDefinition newInpValInfo Nothing
                   )
                   maybeNewGType))
       schemaArguments)

-- | Strip a value type completely, or modify it, if the given value
-- is atomic-ish.
stripValue
  :: (Backend b)
  => RemoteRelationship b
  -> RemoteSchemaIntrospection
  -> G.GType
  -> G.Value G.Name
  -> StateT
       (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
       (Either (ValidationError b))
       (Maybe G.GType)
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

-- | Produce a new type for the list, or strip it entirely.
stripList
  :: (Backend b)
  => RemoteRelationship b
  -> RemoteSchemaIntrospection
  -> G.GType
  -> G.Value G.Name
  -> StateT
       (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
       (Either (ValidationError b))
       (Maybe G.GType)
stripList remoteRelationshipName types originalOuterGType value =
  case originalOuterGType of
    G.TypeList nullability innerGType -> do
      maybeNewInnerGType <- stripValue remoteRelationshipName types innerGType value
      pure (G.TypeList nullability <$> maybeNewInnerGType)
    _ -> lift (Left (InvalidGTypeForStripping originalOuterGType))

-- | Produce a new type for the given InpValInfo, modified by
-- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- object.
stripObject
  :: (Backend b)
  => RemoteRelationship b
  -> RemoteSchemaIntrospection
  -> G.GType
  -> HashMap G.Name (G.Value G.Name)
  -> StateT
       (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
       (Either (ValidationError b))
       G.GType
stripObject remoteRelationshipName schemaDoc originalGtype templateArguments =
  case originalGtype of
    G.TypeNamed nullability originalNamedType ->
      case lookupType schemaDoc (G.getBaseType originalGtype) of
        Just (G.TypeDefinitionInputObject originalInpObjTyInfo) -> do
          let originalSchemaArguments =
                mapFromL (G._ivdName . _rsitdDefinition) $ G._iotdValueDefinitions originalInpObjTyInfo
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
renameTypeForRelationship :: (Backend b) => RemoteRelationship b -> Text -> Text
renameTypeForRelationship rtr text =
  text <> "_remote_rel_" <> name
  where name = toTxt (rtrTable rtr) <> remoteRelationshipNameToText (rtrName rtr)

-- | Rename a type.
renameNamedType :: (Text -> Text) -> G.Name -> G.Name
renameNamedType rename =
  G.unsafeMkName . rename . G.unName

-- | Convert a field name to a variable name.
hasuraFieldToVariable
  :: (Backend b, MonadError (ValidationError b) m)
  => (DBJoinField b)
  -> m G.Name
hasuraFieldToVariable hasuraField = do
  let fieldText = case hasuraField of
        JoinColumn columnInfo               -> toTxt $ pgiColumn columnInfo
        JoinComputedField computedFieldInfo -> toTxt $ _scfName computedFieldInfo
  G.mkName fieldText `onNothing` throwError (InvalidGraphQLName fieldText)

-- | Lookup the field in the schema.
lookupField
  :: (MonadError (ValidationError b) m)
  => G.Name
  -> G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> m (G.FieldDefinition RemoteSchemaInputValueDefinition)
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
  :: (Backend b, MonadError (ValidationError b) m)
  => HM.HashMap G.Name RemoteSchemaInputValueDefinition
  -> HM.HashMap G.Name (G.Value G.Name)
  -> HM.HashMap G.Name (DBJoinField b)
  -> RemoteSchemaIntrospection
  -> m ()
validateRemoteArguments expectedArguments providedArguments permittedVariables schemaDocument = do
  traverse_ validateProvided (HM.toList providedArguments)
  -- Not neccessary to validate if all required args are provided in the relationship
  -- traverse validateExpected (HM.toList expectedArguments)
  where
    validateProvided (providedName, providedValue) =
      case HM.lookup providedName expectedArguments of
        Nothing -> throwError (NoSuchArgumentForRemote providedName)
        Just (G._ivdType . _rsitdDefinition -> expectedType) ->
          validateType permittedVariables providedValue expectedType schemaDocument

unwrapGraphQLType :: G.GType -> G.GType
unwrapGraphQLType = \case
  G.TypeList _ lt -> lt
  nt              -> nt

-- | Validate a value against a type.
validateType
  :: (Backend b, MonadError (ValidationError b) m)
  => HM.HashMap G.Name (DBJoinField b)
  -> G.Value G.Name
  -> G.GType
  -> RemoteSchemaIntrospection
  -> m ()
validateType permittedVariables value expectedGType schemaDocument =
  case value of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> throwError (InvalidVariable variable permittedVariables)
        Just fieldInfo -> do
          namedType <- dbJoinFieldToNamedType fieldInfo
          isTypeCoercible (mkGraphQLType namedType) expectedGType
    G.VInt {} -> do
      let intScalarGType = mkGraphQLType intScalar
      isTypeCoercible intScalarGType expectedGType
    G.VFloat {} -> do
      let floatScalarGType = mkGraphQLType floatScalar
      isTypeCoercible floatScalarGType expectedGType
    G.VBoolean {} -> do
      let boolScalarGType = mkGraphQLType boolScalar
      isTypeCoercible boolScalarGType expectedGType
    G.VNull -> throwError NullNotAllowedHere
    G.VString {} -> do
      let stringScalarGType = mkGraphQLType stringScalar
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
                         mapFromL (G._ivdName . _rsitdDefinition) $ (G._iotdValueDefinitions inpObjTypeInfo)
                   in
                   case HM.lookup name objectTypeDefnsMap of
                     Nothing -> throwError $ NoSuchArgumentForRemote name
                     Just (G._ivdType . _rsitdDefinition -> expectedType) ->
                       validateType permittedVariables val expectedType schemaDocument
                 _ -> do
                   throwError $ InvalidType (mkGraphQLType name) "not an input object type")
  where
    mkGraphQLType =
      G.TypeNamed (G.Nullability False)

isTypeCoercible
  :: (MonadError (ValidationError b) m)
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

assertListType
  :: (MonadError (ValidationError b) m)
  => G.GType
  -> m ()
assertListType actualType =
  unless (G.isListType actualType)
    (throwError $ InvalidType actualType "is not a list type")

-- | Convert a field info to a named type, if possible.
dbJoinFieldToNamedType
  :: forall b m .
    (Backend b, MonadError (ValidationError b) m)
  => DBJoinField b
  -> m G.Name
dbJoinFieldToNamedType hasuraField = do
  scalarType <- case hasuraField of
    JoinColumn pci -> case pgiType pci of
        ColumnScalar scalarType -> pure scalarType
        _                       -> throwError UnsupportedEnum
    JoinComputedField cfi -> pure $ _scfType cfi
        -- CFRScalar scalarType -> pure scalarType
        -- CFRSetofTable table  -> throwError $ UnsupportedTableComputedField table $ _cfiName cfi
  onLeft (scalarTypeGraphQLName @b scalarType) $
    const $ throwError $ CannotGenerateGraphQLTypeName scalarType

getBaseTyWithNestedLevelsCount :: G.GType -> (G.Name, Int)
getBaseTyWithNestedLevelsCount ty = go ty 0
  where
    go :: G.GType -> Int -> (G.Name, Int)
    go gType ctr =
      case gType of
        G.TypeNamed _ n      -> (n, ctr)
        G.TypeList  _ gType' -> go gType' (ctr + 1)
