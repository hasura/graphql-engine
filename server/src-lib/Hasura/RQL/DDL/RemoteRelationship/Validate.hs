{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( validateRemoteRelationship
  , validateErrorToText
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Validation
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude                hiding (first)
import           Hasura.RQL.Types
import           Hasura.Server.Utils           (makeReasonMessage)
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

-- | An error validating the remote relationship.
data ValidationError
  = RemoteSchemaNotFound !RemoteSchemaName
  | CouldntFindRemoteField !G.Name !G.NamedType
  | FieldNotFoundInRemoteSchema !G.Name
  | NoSuchArgumentForRemote !G.Name
  | MissingRequiredArgument !G.Name
  | TypeNotFound !G.NamedType
  | TableNotFound !QualifiedTable
  | TableFieldNonexistent !QualifiedTable !FieldName
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType!T.Text
  | InvalidVariable !G.Variable !(HM.HashMap G.Variable PGColumnInfo)
  | NullNotAllowedHere
  | InvalidGTypeForStripping !G.GType
  | UnsupportedMultipleElementLists
  | UnsupportedEnum
  deriving (Show, Eq)

validateErrorToText :: NE.NonEmpty ValidationError -> Text
validateErrorToText (toList -> errs) =
  "cannot validate remote relationship " <> makeReasonMessage errs errorToText
  where
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
        "expected type " <> getBaseTy expTy <<> " but got " <>> getBaseTy actualTy
      InvalidType ty err ->
        "type " <> getBaseTy ty <<> err
      InvalidVariable var _ ->
        "variable " <> G.unVariable var <<> " is not found"
      NullNotAllowedHere ->
        "null is not allowed here"
      InvalidGTypeForStripping ty ->
        "type " <> getBaseTy ty <<> " is invalid for stripping"
      UnsupportedMultipleElementLists ->
        "multiple elements in list value is not supported"
      UnsupportedEnum ->
        "enum value is not supported"

-- | Validate a remote relationship given a context.
validateRemoteRelationship ::
     RemoteRelationship
  -> RemoteSchemaMap
  -> [PGColumnInfo]
  -> Either (NonEmpty ValidationError) (RemoteFieldInfo, TypeMap)
validateRemoteRelationship remoteRelationship remoteSchemaMap pgColumns = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
      table = rtrTable remoteRelationship
  hasuraFields <- forM (toList $ rtrHasuraFields remoteRelationship) $
    \fieldName -> case find ((==) fieldName . fromPGCol . pgiColumn) pgColumns of
                    Nothing -> Left $ pure $ TableFieldNonexistent table fieldName
                    Just r  -> pure r
  case HM.lookup remoteSchemaName remoteSchemaMap of
    Nothing -> Left $ pure $ RemoteSchemaNotFound remoteSchemaName
    Just (RemoteSchemaCtx _ gctx rsi) -> do
      (_leafTyInfo, leafGType, (leafParamMap, leafTypeMap)) <-
        foldl
          (\eitherObjTyInfoAndTypes fieldCall ->
             case eitherObjTyInfoAndTypes of
               Left err -> Left err
               Right (objTyInfo, _, (_, typeMap)) -> do
                 objFldInfo <- lookupField (fcName fieldCall) objTyInfo
                 case _fiLoc objFldInfo of
                   TLHasuraType ->
                     Left
                       (pure (FieldNotFoundInRemoteSchema (fcName fieldCall)))
                   TLCustom ->
                     Left
                       (pure (FieldNotFoundInRemoteSchema (fcName fieldCall)))
                   TLRemoteType {} -> do
                     let providedArguments =
                           remoteArgumentsToMap (fcArguments fieldCall)
                     toEither
                       (validateRemoteArguments
                          (_fiParams objFldInfo)
                          providedArguments
                          (HM.fromList
                             (map
                                (first pgColumnToVariable)
                                (HM.toList $ mapFromL (pgiColumn) pgColumns)))
                          (GS._gTypes gctx))
                     (newParamMap, newTypeMap) <-
                       first
                         pure
                         (runStateT
                            (stripInMap
                               remoteRelationship
                               (GS._gTypes gctx)
                               (_fiParams objFldInfo)
                               providedArguments)
                            typeMap)
                     innerObjTyInfo <-
                       if isObjType (GS._gTypes gctx) objFldInfo
                         then getTyInfoFromField (GS._gTypes gctx) objFldInfo
                         else if isValidType (GS._gTypes gctx) objFldInfo
                                then pure objTyInfo
                                else (Left
                                        (pure
                                           (InvalidType
                                              (_fiTy objFldInfo)
                                              "only output type is expected")))
                     pure
                       ( innerObjTyInfo
                       , _fiTy objFldInfo
                       , (newParamMap, newTypeMap)))
          (pure
             ( GS._gQueryRoot gctx
             , G.toGT (_otiName $ GS._gQueryRoot gctx)
             , (mempty, mempty)))
          (unRemoteFields $ rtrRemoteField remoteRelationship)
      pure
        ( RemoteFieldInfo
            { _rfiName = rtrName remoteRelationship
            , _rfiGType = leafGType
            , _rfiParamMap = leafParamMap
            , _rfiHasuraFields = HS.fromList hasuraFields
            , _rfiRemoteFields = unRemoteFields $ rtrRemoteField remoteRelationship
            , _rfiRemoteSchema = rsi
            }
        , leafTypeMap)
  where
    getTyInfoFromField types field =
      let baseTy = getBaseTy (_fiTy field)
          fieldName = _fiName field
          typeInfo = HM.lookup baseTy types
       in case typeInfo of
            Just (TIObj objTyInfo) -> pure objTyInfo
            _                      -> Left (pure (FieldNotFoundInRemoteSchema fieldName))
    isObjType types field =
      let baseTy = getBaseTy (_fiTy field)
          typeInfo = HM.lookup baseTy types
       in case typeInfo of
            Just (TIObj _) -> True
            _              -> False

    isValidType types field =
      let baseTy = getBaseTy (_fiTy field)
          typeInfo = HM.lookup baseTy types
       in case typeInfo of
            Just (TIScalar _) -> True
            Just (TIEnum   _) -> True
            Just (TIIFace _)  -> True
            Just (TIUnion _)  -> True
            _                 -> False

    remoteArgumentsToMap =
      HM.fromList .
      map (\field -> (G._ofName field, G._ofValue field)) .
      getRemoteArguments

-- | Return a map with keys deleted whose template argument is
-- specified as an atomic (variable, constant), keys which are kept
-- have their values modified by 'stripObject' or 'stripList'.
stripInMap ::
     RemoteRelationship -> HM.HashMap G.NamedType TypeInfo
  -> HM.HashMap G.Name InpValInfo
  -> HM.HashMap G.Name G.Value
  -> StateT (HM.HashMap G.NamedType TypeInfo) (Either ValidationError) (HM.HashMap G.Name InpValInfo)
stripInMap remoteRelationshipName types schemaArguments templateArguments =
  fmap
    (HM.mapMaybe id)
    (HM.traverseWithKey
       (\name inpValInfo ->
          case HM.lookup name templateArguments of
            Nothing -> pure (Just inpValInfo)
            Just value -> do
              maybeNewGType <- stripValue remoteRelationshipName types (_iviType inpValInfo) value
              pure
                (fmap
                   (\newGType -> inpValInfo {_iviType = newGType})
                   maybeNewGType))
       schemaArguments)

-- | Strip a value type completely, or modify it, if the given value
-- is atomic-ish.
stripValue ::
     RemoteRelationship -> HM.HashMap G.NamedType TypeInfo
  -> G.GType
  -> G.Value
  -> StateT (HM.HashMap G.NamedType TypeInfo) (Either ValidationError) (Maybe G.GType)
stripValue remoteRelationshipName types gtype value = do
  case value of
    G.VVariable {} -> pure Nothing
    G.VInt {} -> pure Nothing
    G.VFloat {} -> pure Nothing
    G.VString {} -> pure Nothing
    G.VBoolean {} -> pure Nothing
    G.VNull {} -> pure Nothing
    G.VEnum {} -> pure Nothing
    G.VList (G.ListValueG values) ->
      case values of
        []       -> pure Nothing
        [gvalue] -> stripList remoteRelationshipName types gtype gvalue
        _        -> lift (Left UnsupportedMultipleElementLists)
    G.VObject (G.unObjectValue -> keypairs) ->
      fmap Just (stripObject remoteRelationshipName types gtype keypairs)

-- | Produce a new type for the list, or strip it entirely.
stripList ::
     RemoteRelationship
  -> HM.HashMap G.NamedType TypeInfo
  -> G.GType
  -> G.Value
  -> StateT (HM.HashMap G.NamedType TypeInfo) (Either ValidationError) (Maybe G.GType)
stripList remoteRelationshipName types originalOuterGType value =
  case originalOuterGType of
    G.TypeList nullability (G.ListType innerGType) -> do
      maybeNewInnerGType <- stripValue remoteRelationshipName types innerGType value
      pure
        (fmap
           (\newGType -> G.TypeList nullability (G.ListType newGType))
           maybeNewInnerGType)
    _ -> lift (Left (InvalidGTypeForStripping originalOuterGType))

-- | Produce a new type for the given InpValInfo, modified by
-- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- object.
stripObject ::
     RemoteRelationship -> HM.HashMap G.NamedType TypeInfo
  -> G.GType
  -> [G.ObjectFieldG G.Value]
  -> StateT (HM.HashMap G.NamedType TypeInfo) (Either ValidationError) G.GType
stripObject remoteRelationshipName types originalGtype keypairs =
  case originalGtype of
    G.TypeNamed nullability originalNamedType ->
      case HM.lookup (getBaseTy originalGtype) types of
        Just (TIInpObj originalInpObjTyInfo) -> do
          let originalSchemaArguments = _iotiFields originalInpObjTyInfo
              newNamedType =
                renameNamedType
                  (renameTypeForRelationship remoteRelationshipName)
                  originalNamedType
          newSchemaArguments <-
            stripInMap
              remoteRelationshipName
              types
              originalSchemaArguments
              templateArguments
          let newInpObjTyInfo =
                originalInpObjTyInfo
                  {_iotiFields = newSchemaArguments, _iotiName = newNamedType}
              newGtype = G.TypeNamed nullability newNamedType
          modify (HM.insert newNamedType (TIInpObj newInpObjTyInfo))
          pure newGtype
        _ -> lift (Left (InvalidGTypeForStripping originalGtype))
    _ -> lift (Left (InvalidGTypeForStripping originalGtype))
  where
    templateArguments :: HM.HashMap G.Name G.Value
    templateArguments =
      HM.fromList (map (\(G.ObjectFieldG key val) -> (key, val)) keypairs)

-- | Produce a new name for a type, used when stripping the schema
-- types for a remote relationship.
-- TODO: Consider a separator character to avoid conflicts.
renameTypeForRelationship :: RemoteRelationship -> Text -> Text
renameTypeForRelationship rtr text =
  text <> "_remote_rel_" <> name
  where name = schema <> "_" <> table <> remoteRelationshipNameToText (rtrName rtr)
        QualifiedObject (SchemaName schema) (TableName table) = rtrTable rtr

-- | Rename a type.
renameNamedType :: (Text -> Text) -> G.NamedType -> G.NamedType
renameNamedType rename (G.NamedType (G.Name text)) =
  G.NamedType (G.Name (rename text))

-- | Convert a field name to a variable name.
pgColumnToVariable :: PGCol -> G.Variable
pgColumnToVariable = G.Variable . G.Name . getPGColTxt

-- | Lookup the field in the schema.
lookupField ::
     G.Name
  -> ObjTyInfo
  -> Either (NonEmpty ValidationError) ObjFldInfo
lookupField name objFldInfo = viaObject objFldInfo
  where
    viaObject =
      maybe (Left (pure (CouldntFindRemoteField name $ _otiName objFldInfo))) pure .
      HM.lookup name .
      _otiFields

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments ::
     HM.HashMap G.Name InpValInfo
  -> HM.HashMap G.Name G.Value
  -> HM.HashMap G.Variable PGColumnInfo
  -> HM.HashMap G.NamedType TypeInfo
  -> Validation (NonEmpty ValidationError) ()
validateRemoteArguments expectedArguments providedArguments permittedVariables types = do
  traverse validateProvided (HM.toList providedArguments)
  -- Not neccessary to validate if all required args are provided in the relationship
  -- traverse validateExpected (HM.toList expectedArguments)
  pure ()
  where
    validateProvided (providedName, providedValue) =
      case HM.lookup providedName expectedArguments of
        Nothing -> Failure (pure (NoSuchArgumentForRemote providedName))
        Just (_iviType -> expectedType) ->
          validateType permittedVariables providedValue expectedType types
    -- validateExpected (expectedKey, expectedInpValInfo) =
    --   if G.isNullable (_iviType expectedInpValInfo)
    --     then pure ()
    --     else case _iviDefVal expectedInpValInfo of
    --            Just {} -> pure ()
    --            Nothing ->
    --              case HM.lookup expectedKey providedArguments of
    --                Nothing ->
    --                  Failure (pure (MissingRequiredArgument expectedKey))
    --                Just {} -> pure ()


-- | Validate a value against a type.
validateType ::
     HM.HashMap G.Variable PGColumnInfo
  -> G.Value
  -> G.GType
  -> HM.HashMap G.NamedType TypeInfo
  -> Validation (NonEmpty ValidationError) ()
validateType permittedVariables value expectedGType types =
  case value of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> Failure (pure (InvalidVariable variable permittedVariables))
        Just fieldInfo ->
          bindValidation
            (columnInfoToNamedType fieldInfo)
            (\actualNamedType -> isTypeCoercible (G.toGT actualNamedType) expectedGType)
    G.VInt {} -> do
      let intScalarGType = G.toGT $ mkScalarTy PGInteger
      isTypeCoercible intScalarGType expectedGType
    G.VFloat {} -> do
      let floatScalarGType = G.toGT $ mkScalarTy PGFloat
      isTypeCoercible floatScalarGType expectedGType
    G.VBoolean {} -> do
      let boolScalarGType = G.toGT $ mkScalarTy PGBoolean
      isTypeCoercible boolScalarGType expectedGType
    G.VNull -> Failure $ pure NullNotAllowedHere
    G.VString {} -> do
      let stringScalarGType = G.toGT $ mkScalarTy PGText
      isTypeCoercible stringScalarGType expectedGType
    G.VEnum _ -> Failure $ pure UnsupportedEnum
    G.VList (G.unListValue -> values) -> do
      case values of
        []  -> pure ()
        [_] -> pure ()
        _   -> Failure (pure UnsupportedMultipleElementLists)
      (assertListType expectedGType)
      (flip
         traverse_
         values
         (\val ->
            validateType permittedVariables val (unwrapTy expectedGType) types))
      pure ()
    G.VObject (G.unObjectValue -> values) ->
      flip
        traverse_
        values
        (\(G.ObjectFieldG name val) ->
           let expectedNamedType = getBaseTy expectedGType
           in
           case HM.lookup expectedNamedType types of
             Nothing -> Failure (pure $ TypeNotFound expectedNamedType)
             Just typeInfo ->
               case typeInfo of
                 TIInpObj inpObjTypeInfo ->
                   case HM.lookup name (_iotiFields inpObjTypeInfo) of
                     Nothing -> Failure (pure $ NoSuchArgumentForRemote name)
                     Just (_iviType -> expectedType) ->
                       validateType permittedVariables val expectedType types
                 _ ->
                   Failure
                     (pure $
                      InvalidType
                        (G.toGT $ G.NamedType name)
                        "not an input object type"))

isTypeCoercible
  :: G.GType
  -> G.GType
  -> Validation (NonEmpty ValidationError) ()
isTypeCoercible actualType expectedType =
  -- The GraphQL spec says that, a singleton type can be coerced into  an array
  -- type. Which means that if the 'actualType' is a singleton type, like
  -- 'Int' we should be able to join this with a remote node, which expects an
  -- input argument of type '[Int]'
  -- http://spec.graphql.org/June2018/#sec-Type-System.List
  let (actualBaseType, actualNestingLevel) = getBaseTyWithNestedLevelsCount actualType
      (expectedBaseType, expectedNestingLevel) = getBaseTyWithNestedLevelsCount expectedType
  in
  if | actualBaseType /= expectedBaseType -> raiseValidationError
       -- we cannot coerce two types with different nesting levels,
       -- for example, we cannot coerce [Int] to [[Int]]
     | (actualNestingLevel == expectedNestingLevel || actualNestingLevel == 0) -> pure ()
     | otherwise -> raiseValidationError
     where
       raiseValidationError = Failure $ pure $ ExpectedTypeButGot expectedType actualType

assertListType :: G.GType -> Validation (NonEmpty ValidationError) ()
assertListType actualType =
  (when (not $ isListType' actualType)
    (Failure (pure $ InvalidType actualType "is not a list type")))

-- | Convert a field info to a named type, if possible.
columnInfoToNamedType :: PGColumnInfo -> Validation (NonEmpty ValidationError) G.NamedType
columnInfoToNamedType pci = case pgiType pci of
  PGColumnScalar scalarType -> pure $ mkScalarTy scalarType
  _                         -> Failure $ pure UnsupportedEnum
