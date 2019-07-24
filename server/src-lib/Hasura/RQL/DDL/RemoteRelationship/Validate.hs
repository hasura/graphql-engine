{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( getCreateRemoteRelationshipValidation
  , validateRelationship
  , validateRemoteArguments
  , ValidationError(..)
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Validation
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

-- | An error validating the remote relationship.
data ValidationError
  = CouldntFindRemoteField G.Name ObjTyInfo
  | FieldNotFoundInRemoteSchema G.Name
  | NoSuchArgumentForRemote G.Name
  | MissingRequiredArgument G.Name
  | TypeNotFound G.NamedType
  | TableNotFound !QualifiedTable
  | TableFieldNonexistent !QualifiedTable !FieldName
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType!T.Text
  | InvalidVariable G.Variable (HM.HashMap G.Variable FieldInfo)
  | NullNotAllowedHere
  | ForeignRelationshipsNotAllowedInRemoteVariable !RelInfo
  | RemoteFieldsNotAllowedInArguments !RemoteField
  | UnsupportedArgumentType G.Value
  | InvalidGTypeForStripping !G.GType
  | UnsupportedMultipleElementLists
  deriving (Show, Eq)

-- | Get a validation for the remote relationship proposal.
getCreateRemoteRelationshipValidation ::
     (QErrM m, CacheRM m)
  => RemoteRelationship
  -> m (Either (NonEmpty ValidationError) (RemoteField, TypeMap))
getCreateRemoteRelationshipValidation createRemoteRelationship = do
  schemaCache <- askSchemaCache
  pure
    (validateRelationship
       createRemoteRelationship
       (scDefaultRemoteGCtx schemaCache)
       (scTables schemaCache))

-- | Validate a remote relationship given a context.
validateRelationship ::
     RemoteRelationship
  -> GC.GCtx
  -> HM.HashMap QualifiedTable TableInfo
  -> Either (NonEmpty ValidationError) (RemoteField, TypeMap)
validateRelationship remoteRelationship gctx tables = do
  case HM.lookup tableName tables of
    Nothing -> Left (pure (TableNotFound tableName))
    Just table -> do
      fieldInfos <-
        fmap
          HM.fromList
          (traverse
             (\fieldName ->
                case HM.lookup fieldName (tiFieldInfoMap table) of
                  Nothing ->
                    Left (pure (TableFieldNonexistent tableName fieldName))
                  Just fieldInfo -> pure (fieldName, fieldInfo))
             (toList (rtrHasuraFields remoteRelationship)))
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
                   TLRemoteType {} -> do
                     let providedArguments =
                           remoteArgumentsToMap (fcArguments fieldCall)
                     toEither
                       (validateRemoteArguments
                          (_fiParams objFldInfo)
                          providedArguments
                          (HM.fromList
                             (map
                                (first fieldNameToVariable)
                                (HM.toList fieldInfos)))
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
                       else if isScalarType (GS._gTypes gctx) objFldInfo
                       then pure objTyInfo
                       else (Left (pure (InvalidType (_fiTy objFldInfo) "only objects or scalar types expected")))
                     pure
                       ( innerObjTyInfo
                       , _fiTy objFldInfo
                       , (newParamMap, newTypeMap)))
          (pure
             ( GS._gQueryRoot gctx
             , G.toGT (_otiName $ GS._gQueryRoot gctx)
             , (mempty, mempty)))
          (rtrRemoteFields remoteRelationship)
      pure
        ( RemoteField
            { rmfRemoteRelationship = remoteRelationship
            , rmfGType = leafGType
            , rmfParamMap = leafParamMap
            }
        , leafTypeMap)
  where
    tableName = rtrTable remoteRelationship
    getTyInfoFromField types field =
      let baseTy = getBaseTy (_fiTy field)
          fieldName = _fiName field
          typeInfo = HM.lookup baseTy types
       in case typeInfo of
            Just (TIObj objTyInfo) -> pure objTyInfo
            _ -> Left (pure (FieldNotFoundInRemoteSchema fieldName))
    isObjType types field =
      let baseTy = getBaseTy (_fiTy field)
          typeInfo = HM.lookup baseTy types
      in case typeInfo of
           Just (TIObj _) -> True
           _              -> False
    isScalarType types field =
      let baseTy = getBaseTy (_fiTy field)
          typeInfo = HM.lookup baseTy types
      in case typeInfo of
           Just (TIScalar _) -> True
           _                 -> False

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
  where name = schema <> "_" <> table <> rrname
        QualifiedObject (SchemaName schema) (TableName table) = rtrTable rtr
        RemoteRelationshipName rrname = rtrName rtr

-- | Rename a type.
renameNamedType :: (Text -> Text) -> G.NamedType -> G.NamedType
renameNamedType rename (G.NamedType (G.Name text)) =
  G.NamedType (G.Name (rename text))

-- | Convert a field name to a variable name.
fieldNameToVariable :: FieldName -> G.Variable
fieldNameToVariable = G.Variable . G.Name . getFieldNameTxt

-- | Lookup the field in the schema.
lookupField ::
     G.Name
  -> ObjTyInfo
  -> Either (NonEmpty ValidationError) ObjFldInfo
lookupField name objFldInfo = viaObject objFldInfo
  where
    viaObject =
      maybe (Left (pure (CouldntFindRemoteField name objFldInfo))) pure .
      HM.lookup name .
      _otiFields

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments ::
     HM.HashMap G.Name InpValInfo
  -> HM.HashMap G.Name G.Value
  -> HM.HashMap G.Variable FieldInfo
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
     HM.HashMap G.Variable FieldInfo
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
            (fieldInfoToNamedType fieldInfo)
            (\actualNamedType -> assertType (G.toGT actualNamedType) expectedGType)
    G.VInt {} -> assertType (G.toGT $ mkScalarTy PGInteger) expectedGType
    G.VFloat {} -> assertType (G.toGT $ mkScalarTy PGFloat) expectedGType
    G.VBoolean {} -> assertType (G.toGT $ mkScalarTy PGBoolean) expectedGType
    G.VNull -> Failure (pure NullNotAllowedHere)
    G.VString {} -> assertType (G.toGT $ mkScalarTy PGText) expectedGType
    v@(G.VEnum _) -> Failure (pure (UnsupportedArgumentType v))
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

assertType :: G.GType -> G.GType -> Validation (NonEmpty ValidationError) ()
assertType actualType expectedType = do
  -- check if both are list types or both are named types
  (when
     (isListType actualType /= isListType expectedType)
     (Failure (pure $ ExpectedTypeButGot expectedType actualType)))
  -- if list type then check over unwrapped type, else check base types
  if isListType actualType
    then assertType (unwrapTy actualType) (unwrapTy expectedType)
    else (when
            (getBaseTy actualType /= getBaseTy expectedType)
            (Failure (pure $ ExpectedTypeButGot expectedType actualType)))
  pure ()

assertListType :: G.GType -> Validation (NonEmpty ValidationError) ()
assertListType actualType =
  (when (not $ isListType actualType)
    (Failure (pure $ InvalidType actualType "is not a list type")))

-- | Convert a field info to a named type, if possible.
fieldInfoToNamedType ::
     FieldInfo
  -> Validation (NonEmpty ValidationError) G.NamedType
fieldInfoToNamedType =
  \case
    FIColumn pgColInfo -> pure (mkScalarTy (pgiType pgColInfo))
    FIRelationship relInfo ->
      Failure (pure (ForeignRelationshipsNotAllowedInRemoteVariable relInfo))
    FIRemote remoteField ->
      Failure (pure (RemoteFieldsNotAllowedInArguments remoteField))

-- | Reify the constructors to an Either.
isListType :: G.GType -> Bool
isListType =
  \case
    G.TypeNamed {} -> False
    G.TypeList {} -> True
