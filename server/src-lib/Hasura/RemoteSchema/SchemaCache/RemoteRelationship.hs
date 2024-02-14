{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.
module Hasura.RemoteSchema.SchemaCache.RemoteRelationship
  ( validateToSchemaRelationship,
    errorToText,
  )
where

import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as HS
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Name qualified as Name
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Common
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.Server.Utils (englishList)
import Language.GraphQL.Draft.Syntax qualified as G

-- | An error validating the remote relationship.
data ValidationError
  = RemoteSchemaNotFound RemoteSchemaName
  | CouldntFindRemoteField G.Name G.Name
  | FieldNotFoundInRemoteSchema G.Name
  | NoSuchArgumentForRemote G.Name
  | MissingRequiredArgument G.Name
  | TypeNotFound G.Name
  | JoinFieldNonExistent LHSIdentifier FieldName (HS.HashSet FieldName)
  | ExpectedTypeButGot G.GType G.GType
  | InvalidType G.GType Text
  | InvalidVariable G.Name (HS.HashSet G.Name)
  | NullNotAllowedHere
  | InvalidGTypeForStripping G.GType
  | UnsupportedMultipleElementLists
  | UnsupportedEnum
  | InvalidGraphQLName Text
  | IDTypeJoin G.Name
  | -- | TODO: Can this be made not reachable?
    -- This is the case where the type of the columns that are mapped do not
    -- have a graphql representation. This case is probably not reachable as
    -- having a db type which can't be representable in GraphQL should definitely
    -- fail the entire schema generation process
    CannotGenerateGraphQLTypeName G.Name
  deriving (Show, Eq)

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
  JoinFieldNonExistent (LHSIdentifier lhs) fieldName allowedJoinFields ->
    let helpText =
          case NE.nonEmpty $ map dquote $ toList allowedJoinFields of
            Nothing -> ""
            Just allowedFields -> ", the allowed fields are " <> englishList "or" allowedFields
     in "field with name "
          <> fieldName
          <<> "is not provided by the lhs entity"
          <>> lhs
          <<> "for defining a join condition"
          <> helpText
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
    "Only ID, Int, uuid or String scalar types can be joined to the ID type, but received " <>> typeName
  CannotGenerateGraphQLTypeName typeName ->
    "the name of the scalar type "
      <> toTxt typeName
      <> " is not a valid GraphQL identifier, "
      <> " so columns of such type cannot be used in a remote schema mapping "

-- | Validate a remote schema relationship given a context.
validateToSchemaRelationship ::
  (MonadError ValidationError m) =>
  ToSchemaRelationshipDef ->
  LHSIdentifier ->
  RelName ->
  (RemoteSchemaInfo, IntrospectionResult) ->
  HashMap.HashMap FieldName joinField ->
  m (HashMap.HashMap FieldName joinField, RemoteSchemaFieldInfo)
validateToSchemaRelationship schema lhsIdentifier name (remoteSchemaInfo, introspectionResult) lhsJoinFields = do
  let remoteSchemaName = _trrdRemoteSchema schema
  requiredLHSJoinFields <- forM (toList $ _trrdLhsFields schema) $ \fieldName -> do
    fmap (fieldName,)
      $ onNothing (HashMap.lookup fieldName lhsJoinFields)
      $ throwError
      $ JoinFieldNonExistent lhsIdentifier fieldName
      $ HashMap.keysSet lhsJoinFields
  hasuraFieldsVariablesMap <-
    fmap HashMap.fromList $ for requiredLHSJoinFields $ \(fieldName, field) -> (,field) <$> hasuraFieldToVariable fieldName
  let schemaDoc = irDoc introspectionResult
      queryRootName = irQueryRoot introspectionResult
  queryRoot <-
    onNothing (lookupObject schemaDoc queryRootName)
      $ throwError
      $ FieldNotFoundInRemoteSchema queryRootName
  (_, (leafParamMap, leafTypeMap)) <-
    foldlM
      (buildRelationshipTypeInfo hasuraFieldsVariablesMap schemaDoc)
      (queryRoot, (mempty, mempty))
      (unRemoteFields $ _trrdRemoteField schema)
  pure
    $ ( HashMap.fromList requiredLHSJoinFields,
        RemoteSchemaFieldInfo
          { _rrfiName = name,
            _rrfiParamMap = leafParamMap,
            _rrfiRemoteFields = _trrdRemoteField schema,
            _rrfiRemoteSchema = remoteSchemaInfo,
            -- adding the new input types after stripping the values of the
            -- schema document
            _rrfiInputValueDefinitions = HashMap.elems leafTypeMap,
            _rrfiRemoteSchemaName = remoteSchemaName,
            _rrfiLHSIdentifier = lhsIdentifier
          }
      )
  where
    getObjTyInfoFromField ::
      RemoteSchemaIntrospection ->
      G.FieldDefinition RemoteSchemaInputValueDefinition ->
      Maybe (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
    getObjTyInfoFromField schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
       in lookupObject schemaDoc baseTy

    isValidType schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
       in case lookupType schemaDoc baseTy of
            Just (G.TypeDefinitionScalar _) -> True
            Just (G.TypeDefinitionInterface _) -> True
            Just (G.TypeDefinitionUnion _) -> True
            Just (G.TypeDefinitionEnum _) -> True
            _ -> False

    buildRelationshipTypeInfo ::
      (MonadError ValidationError m) =>
      HashMap G.Name joinField ->
      RemoteSchemaIntrospection ->
      ( G.ObjectTypeDefinition RemoteSchemaInputValueDefinition,
        ( HashMap G.Name RemoteSchemaInputValueDefinition,
          HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
        )
      ) ->
      FieldCall ->
      m
        ( G.ObjectTypeDefinition RemoteSchemaInputValueDefinition,
          ( HashMap G.Name RemoteSchemaInputValueDefinition,
            HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
          )
        )
    buildRelationshipTypeInfo hasuraFieldsVariablesMap schemaDoc (objTyInfo, (_, typeMap)) fieldCall = do
      objFldDefinition <- lookupField (fcName fieldCall) objTyInfo
      let providedArguments = getRemoteArguments $ fcArguments fieldCall
      validateRemoteArguments
        (mapFromL (G._ivdName . _rsitdDefinition) (G._fldArgumentsDefinition objFldDefinition))
        providedArguments
        hasuraFieldsVariablesMap
        schemaDoc
      let eitherParamAndTypeMap =
            runStateT
              ( stripInMap
                  name
                  lhsIdentifier
                  schemaDoc
                  (mapFromL (G._ivdName . _rsitdDefinition) (G._fldArgumentsDefinition objFldDefinition))
                  providedArguments
              )
              typeMap
      (newParamMap, newTypeMap) <- onLeft eitherParamAndTypeMap throwError
      innerObjTyInfo <-
        onNothing (getObjTyInfoFromField schemaDoc objFldDefinition)
          $ bool
            ( throwError
                $ InvalidType (G._fldType objFldDefinition) "only output type is expected"
            )
            (pure objTyInfo)
            (isValidType schemaDoc objFldDefinition)
      pure
        ( innerObjTyInfo,
          (newParamMap, newTypeMap)
        )

-- | Return a map with keys deleted whose template argument is
-- specified as an atomic (variable, constant), keys which are kept
-- have their values modified by 'stripObject' or 'stripList'.
-- This function creates the 'HashMap G.Name G.InputValueDefinition' which modifies
-- the original input parameters (if any) of the remote node/table being used. Only
-- list or object types are preserved and other types are stripped off. The object or
-- list types are preserved because they can be merged, if any arguments are
-- provided by the user while querying a remote join field.
stripInMap ::
  RelName ->
  LHSIdentifier ->
  RemoteSchemaIntrospection ->
  HashMap.HashMap G.Name RemoteSchemaInputValueDefinition ->
  HashMap.HashMap G.Name (G.Value G.Name) ->
  StateT
    (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
    (Either ValidationError)
    (HashMap.HashMap G.Name RemoteSchemaInputValueDefinition)
stripInMap relName lhsIdentifier types schemaArguments providedArguments =
  fmap catMaybes
    $ HashMap.traverseWithKey
      ( \name remoteInpValDef@(RemoteSchemaInputValueDefinition inpValInfo _preset) ->
          case HashMap.lookup name providedArguments of
            Nothing -> pure $ Just remoteInpValDef
            Just value -> do
              maybeNewGType <- stripValue relName lhsIdentifier types (G._ivdType inpValInfo) value
              pure
                $ fmap
                  ( \newGType ->
                      let newInpValInfo = inpValInfo {G._ivdType = newGType}
                       in RemoteSchemaInputValueDefinition newInpValInfo Nothing
                  )
                  maybeNewGType
      )
      schemaArguments

-- | Strip a value type completely, or modify it, if the given value
-- is atomic-ish.
stripValue ::
  RelName ->
  LHSIdentifier ->
  RemoteSchemaIntrospection ->
  G.GType ->
  G.Value G.Name ->
  StateT
    (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
    (Either ValidationError)
    (Maybe G.GType)
stripValue name lhsIdentifier types gtype value = do
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
        [] -> pure Nothing
        [gvalue] -> stripList name lhsIdentifier types gtype gvalue
        _ -> lift (Left UnsupportedMultipleElementLists)
    G.VObject keyPairs ->
      fmap Just (stripObject name lhsIdentifier types gtype keyPairs)

-- | Produce a new type for the list, or strip it entirely.
stripList ::
  RelName ->
  LHSIdentifier ->
  RemoteSchemaIntrospection ->
  G.GType ->
  G.Value G.Name ->
  StateT
    (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
    (Either ValidationError)
    (Maybe G.GType)
stripList name lhsIdentifier types originalOuterGType value =
  case originalOuterGType of
    G.TypeList nullability innerGType -> do
      maybeNewInnerGType <- stripValue name lhsIdentifier types innerGType value
      pure (G.TypeList nullability <$> maybeNewInnerGType)
    _ -> lift (Left (InvalidGTypeForStripping originalOuterGType))

-- | Produce a new type for the given InpValInfo, modified by
-- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- object.
stripObject ::
  RelName ->
  LHSIdentifier ->
  RemoteSchemaIntrospection ->
  G.GType ->
  HashMap G.Name (G.Value G.Name) ->
  StateT
    (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
    (Either ValidationError)
    G.GType
stripObject name lhsIdentifier schemaDoc originalGtype templateArguments =
  case originalGtype of
    G.TypeNamed nullability originalNamedType ->
      case lookupType schemaDoc (G.getBaseType originalGtype) of
        Just (G.TypeDefinitionInputObject originalInpObjTyInfo) -> do
          let originalSchemaArguments =
                mapFromL (G._ivdName . _rsitdDefinition) $ G._iotdValueDefinitions originalInpObjTyInfo
          newNamedType <-
            renameTypeForRelationship name lhsIdentifier originalNamedType
          newSchemaArguments <-
            stripInMap
              name
              lhsIdentifier
              schemaDoc
              originalSchemaArguments
              templateArguments
          let newInpObjTyInfo =
                originalInpObjTyInfo
                  { G._iotdValueDefinitions = HashMap.elems newSchemaArguments,
                    G._iotdName = newNamedType
                  }
              newGtype = G.TypeNamed nullability newNamedType
          modify (HashMap.insert newNamedType (G.TypeDefinitionInputObject newInpObjTyInfo))
          pure newGtype
        _ -> lift (Left (InvalidGTypeForStripping originalGtype))
    _ -> lift (Left (InvalidGTypeForStripping originalGtype))

-- | Produce a new name for a type, used when stripping the schema
-- types for a remote relationship.
-- TODO: Consider a separator character to avoid conflicts.
renameTypeForRelationship ::
  (MonadError ValidationError m) =>
  RelName ->
  LHSIdentifier ->
  G.Name ->
  m G.Name
renameTypeForRelationship (relNameToTxt -> relTxt) lhsIdentifier name = do
  lhsName <-
    lhsIdentifierToGraphQLName lhsIdentifier
      `onNothing` throwError (InvalidGraphQLName $ getLHSIdentifier lhsIdentifier)
  relName <-
    G.mkName relTxt
      `onNothing` throwError (InvalidGraphQLName relTxt)
  pure
    $ name
    <> Name.__remote_rel_
    <> lhsName
    <> relName

-- | Convert a field name to a variable name.
hasuraFieldToVariable ::
  (MonadError ValidationError m) =>
  FieldName ->
  m G.Name
hasuraFieldToVariable (FieldName fieldText) = do
  G.mkName fieldText `onNothing` throwError (InvalidGraphQLName fieldText)

-- | Lookup the field in the schema.
lookupField ::
  (MonadError ValidationError m) =>
  G.Name ->
  G.ObjectTypeDefinition RemoteSchemaInputValueDefinition ->
  m (G.FieldDefinition RemoteSchemaInputValueDefinition)
lookupField name objFldInfo = viaObject objFldInfo
  where
    viaObject =
      maybe (throwError (CouldntFindRemoteField name $ G._otdName objFldInfo)) pure
        . lookup name
        . HashMap.toList
        . mapFromL G._fldName
        . G._otdFieldsDefinition

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments ::
  (MonadError ValidationError m) =>
  HashMap.HashMap G.Name RemoteSchemaInputValueDefinition ->
  HashMap.HashMap G.Name (G.Value G.Name) ->
  HashMap.HashMap G.Name joinField ->
  RemoteSchemaIntrospection ->
  m ()
validateRemoteArguments expectedArguments providedArguments permittedVariables schemaDocument = do
  traverse_ validateProvided (HashMap.toList providedArguments)
  where
    -- Not neccessary to validate if all required args are provided in the relationship
    -- traverse validateExpected (HashMap.toList expectedArguments)

    validateProvided (providedName, providedValue) =
      case HashMap.lookup providedName expectedArguments of
        Nothing -> throwError (NoSuchArgumentForRemote providedName)
        Just (G._ivdType . _rsitdDefinition -> expectedType) ->
          validateType permittedVariables providedValue expectedType schemaDocument

unwrapGraphQLType :: G.GType -> G.GType
unwrapGraphQLType = \case
  G.TypeList _ lt -> lt
  nt -> nt

-- | Validate a value against a type.
validateType ::
  (MonadError ValidationError m) =>
  HashMap.HashMap G.Name joinField ->
  G.Value G.Name ->
  G.GType ->
  RemoteSchemaIntrospection ->
  m ()
validateType permittedVariables value expectedGType schemaDocument =
  case value of
    G.VVariable variable ->
      case HashMap.lookup variable permittedVariables of
        Nothing -> throwError (InvalidVariable variable $ HashMap.keysSet permittedVariables)
        -- TODO: check whether the type of lhs join field is allowed
        Just _lhsJoinField -> pure ()
    G.VInt {} -> do
      let intScalarGType = mkGraphQLType GName._Int
      isTypeCoercible intScalarGType expectedGType
    G.VFloat {} -> do
      let floatScalarGType = mkGraphQLType GName._Float
      isTypeCoercible floatScalarGType expectedGType
    G.VBoolean {} -> do
      let boolScalarGType = mkGraphQLType GName._Boolean
      isTypeCoercible boolScalarGType expectedGType
    G.VNull -> throwError NullNotAllowedHere
    G.VString {} -> do
      let stringScalarGType = mkGraphQLType GName._String
      isTypeCoercible stringScalarGType expectedGType
    G.VEnum _ -> throwError UnsupportedEnum
    G.VList values -> do
      case values of
        [] -> pure ()
        [_] -> pure ()
        _ -> throwError UnsupportedMultipleElementLists
      assertListType expectedGType
      for_
        values
        ( \val ->
            validateType permittedVariables val (unwrapGraphQLType expectedGType) schemaDocument
        )
    G.VObject values ->
      for_
        (HashMap.toList values)
        ( \(name, val) ->
            let expectedNamedType = G.getBaseType expectedGType
             in case lookupType schemaDocument expectedNamedType of
                  Nothing -> throwError $ TypeNotFound expectedNamedType
                  Just typeInfo ->
                    case typeInfo of
                      G.TypeDefinitionInputObject inpObjTypeInfo ->
                        let objectTypeDefnsMap =
                              mapFromL (G._ivdName . _rsitdDefinition) $ G._iotdValueDefinitions inpObjTypeInfo
                         in case HashMap.lookup name objectTypeDefnsMap of
                              Nothing -> throwError $ NoSuchArgumentForRemote name
                              Just (G._ivdType . _rsitdDefinition -> expectedType) ->
                                validateType permittedVariables val expectedType schemaDocument
                      _ -> do
                        throwError $ InvalidType (mkGraphQLType name) "not an input object type"
        )
  where
    mkGraphQLType =
      G.TypeNamed (G.Nullability False)

isTypeCoercible ::
  (MonadError ValidationError m) =>
  G.GType ->
  G.GType ->
  m ()
isTypeCoercible actualType expectedType =
  -- The GraphQL spec says that, a singleton type can be coerced into  an array
  -- type. Which means that if the 'actualType' is a singleton type, like
  -- 'Int' we should be able to join this with a remote node, which expects an
  -- input argument of type '[Int]'
  -- http://spec.graphql.org/June2018/#sec-Type-System.List
  let (actualBaseType, actualNestingLevel) = getBaseTyWithNestedLevelsCount actualType
      (expectedBaseType, expectedNestingLevel) = getBaseTyWithNestedLevelsCount expectedType
   in if
        | expectedBaseType == GName._ID ->
            bool
              (throwError $ IDTypeJoin actualBaseType)
              (pure ())
              -- Check under `Input Coercion` https://spec.graphql.org/June2018/#sec-ID
              -- We can also include the `ID` type in the below list but it will be
              -- extraneous because at the time of writing this, we don't generate
              -- the `ID` type in the DB schema
              ( G.unName actualBaseType
                  `elem` ["ID", "Int", "String", "bigint", "smallint", "uuid"]
              )
        | actualBaseType /= expectedBaseType -> raiseValidationError
        -- we cannot coerce two types with different nesting levels,
        -- for example, we cannot coerce [Int] to [[Int]]
        | (actualNestingLevel == expectedNestingLevel || actualNestingLevel == 0) -> pure ()
        | otherwise -> raiseValidationError
  where
    raiseValidationError = throwError $ ExpectedTypeButGot expectedType actualType

assertListType ::
  (MonadError ValidationError m) =>
  G.GType ->
  m ()
assertListType actualType =
  unless
    (G.isListType actualType)
    (throwError $ InvalidType actualType "is not a list type")

getBaseTyWithNestedLevelsCount :: G.GType -> (G.Name, Int)
getBaseTyWithNestedLevelsCount ty = go ty 0
  where
    go :: G.GType -> Int -> (G.Name, Int)
    go gType ctr =
      case gType of
        G.TypeNamed _ n -> (n, ctr)
        G.TypeList _ gType' -> go gType' (ctr + 1)
