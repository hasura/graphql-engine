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
import           Hasura.Prelude                hiding (first)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Hasura.GraphQL.Context        as GC
import qualified Language.GraphQL.Draft.Syntax as G

data ValidationError
  = FieldNotFoundInRemoteSchema G.Name
  | FieldNotFoundInType G.Name !ObjTyInfo
  | TypeNotFoundInRemoteSchema G.NamedType
  | NoSuchArgumentForRemote G.Name
  | MissingRequiredArgument G.Name
  | TableNotFound !QualifiedTable
  | TableFieldNotFound !QualifiedTable !FieldName
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType !T.Text
  | InvalidVariable G.Variable (HM.HashMap G.Variable (FieldInfo PGColumnInfo))
  | NullNotAllowedHere
  | InvalidGTypeForStripping !G.GType
  | UnsupportedArgumentType G.Value
  | UnsupportedForeignRelationship !RelInfo
  | UnsupportedRemoteField !RemoteField
  | UnsupportedEnum
  deriving (Show, Eq)

-- Get a validation for the remote relationship proposal.
-- Success returns (RemoteField, TypeMap) where TypeMap is a map of additional types needed for the RemoteField
getCreateRemoteRelationshipValidation ::
     (QErrM m)
  => RemoteRelationship
  -> RemoteSchemaCtx
  -> TableInfo PGColumnInfo
  -> m (Either (NonEmpty ValidationError) RemoteField)
getCreateRemoteRelationshipValidation remoteRel rsCtx tableInfo = do
  pure
    (validateRelationship
       remoteRel
       (rscGCtx rsCtx)
       tableInfo)

-- | Validate a remote relationship given a context.
validateRelationship ::
     RemoteRelationship
  -> GC.RemoteGCtx
  -> TableInfo PGColumnInfo
  -> Either (NonEmpty ValidationError) RemoteField
validateRelationship remoteRel rGCtx tableInfo = do
  fieldInfos <-
    fmap
      HM.fromList
      (flip traverse (toList (rrHasuraFields remoteRel)) $ \fieldName ->
         case HM.lookup fieldName (_tiFieldInfoMap tableInfo) of
           Nothing -> Left . pure $ TableFieldNotFound tableName fieldName
           Just fieldInfo -> pure (fieldName, fieldInfo))
  let initFieldCalls = NE.init $ rrRemoteFields remoteRel
      leafFieldCall = NE.last $ rrRemoteFields remoteRel
  (leafParentTypeInfo, leafParentTypeMap) <-
    foldl
      (\parentTypeTup fieldCall ->
         case parentTypeTup of
           Left err -> Left err
           Right (objTypeInfo, typeMap) -> do
             (objFldInfo, (_newParamMap, newTypeMap)) <-
               validateFieldCallWith fieldCall fieldInfos objTypeInfo typeMap
             innerTypeInfo <-
               getObjTypeInfoFromField (GC._rgTypes rGCtx) objFldInfo
             pure (innerTypeInfo, newTypeMap))
      (pure (GC._rgQueryRoot rGCtx, mempty))
      initFieldCalls
  (leafObjFldInfo, (leafParamMap, leafTypeMap)) <-
    validateFieldCallWith
      leafFieldCall
      fieldInfos
      leafParentTypeInfo
      leafParentTypeMap
  pure
     RemoteField
        { rfRemoteRelationship = remoteRel
        , rfGType = mkNullable $ _fiTy leafObjFldInfo
        , rfParamMap = leafParamMap
        , rfTypeMap = leafTypeMap }
  where
    tableName = rrTable remoteRel
    mkNullable = \case
      G.TypeNamed _ nt -> G.TypeNamed (G.Nullability True) nt
      G.TypeList _ lt -> G.TypeList (G.Nullability True) lt
    getObjTypeInfoFromField types field = do
      let baseTy = getBaseTy (_fiTy field)
          typeInfoM = HM.lookup baseTy types
      case typeInfoM of
        Just (TIObj objTyInfo) -> pure objTyInfo
        _ ->
          Left . pure $
          InvalidType
            (_fiTy field)
            "only object types expected in nested fields"
    validateFieldCallWith fieldCall fieldInfos objTypeInfo typeMap = do
      objFldInfo <- lookupField (fcName fieldCall) objTypeInfo
      case _fiLoc objFldInfo of
        TLHasuraType ->
          Left . pure $ FieldNotFoundInRemoteSchema (fcName fieldCall)
        TLRemoteType {} -> do
          let providedArguments = remoteArgumentsToMap (fcArguments fieldCall)
          toEither
            (validateRemoteArguments
               (_fiParams objFldInfo)
               providedArguments
               (HM.fromList
                  (map (first fieldNameToVariable) (HM.toList fieldInfos)))
               (GC._rgTypes rGCtx))
          (newParamMap, newTypeMap) <-
            first
              pure
              (runStateT
                 (stripInMap
                    remoteRel
                    (GC._rgTypes rGCtx)
                    (_fiParams objFldInfo)
                    providedArguments)
                 typeMap)
          pure (objFldInfo, (newParamMap, newTypeMap))

-- Return a new param map with keys deleted from join arguments
stripInMap ::
     RemoteRelationship
  -> TypeMap
  -> ParamMap
  -> HM.HashMap G.Name G.Value
  -> StateT (HM.HashMap G.NamedType TypeInfo) (Either ValidationError) ParamMap
stripInMap remoteRel types fieldArguments templateArguments =
  fmap
    (HM.mapMaybe id)
    (HM.traverseWithKey
       (\name inpValInfo ->
          case HM.lookup name templateArguments of
            Nothing -> pure (Just inpValInfo)
            Just value -> do
              maybeNewGType <- stripValue remoteRel types (_iviType inpValInfo) value
              pure
                (fmap
                   (\newGType -> inpValInfo {_iviType = newGType})
                   maybeNewGType))
       fieldArguments)


-- | Strip a value type completely, or modify it if the given value is object type
stripValue ::
     RemoteRelationship
  -> TypeMap
  -> G.GType
  -> G.Value
  -> StateT (TypeMap) (Either ValidationError) (Maybe G.GType)
stripValue remoteRel types gtype value = do
  case value of
    G.VVariable {} -> pure Nothing
    G.VInt {} -> pure Nothing
    G.VFloat {} -> pure Nothing
    G.VString {} -> pure Nothing
    G.VBoolean {} -> pure Nothing
    G.VNull {} -> pure Nothing
    G.VEnum {} -> pure Nothing
    G.VList {} -> pure Nothing
    G.VObject (G.unObjectValue -> keypairs) ->
      fmap Just (stripObject remoteRel types gtype keypairs)

-- | Produce a new type for the given InpValInfo, modified by
-- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- object.
stripObject ::
     RemoteRelationship
  -> TypeMap
  -> G.GType
  -> [G.ObjectFieldG G.Value]
  -> StateT (TypeMap) (Either ValidationError) G.GType
stripObject remoteRel types originalGtype keypairs =
  case originalGtype of
    G.TypeNamed nullability originalNamedType ->
      case HM.lookup (getBaseTy originalGtype) types of
        Just (TIInpObj originalInpObjTyInfo) -> do
          let originalSchemaArguments = _iotiFields originalInpObjTyInfo
              newNamedType =
                renameNamedType
                  remoteRel
                  originalNamedType
          newSchemaArguments <-
            stripInMap
              remoteRel
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
-- NOTE: Not guaranteed to be unique as the generated type name may already exist
renameNamedType :: RemoteRelationship -> G.NamedType -> G.NamedType
renameNamedType remoteRel (G.NamedType (G.Name origName)) =
  G.NamedType (G.Name (renameTypeForRelationship origName))
  where
    renameTypeForRelationship :: Text -> Text
    renameTypeForRelationship text = text <> "_remote_rel_" <> name
      where
        name = schema <> "_" <> table <> "_" <> relName
        QualifiedObject (SchemaName schema) (TableName table) =
          rrTable remoteRel
        RemoteRelationshipName relName = rrName remoteRel

-- | Convert a field name to a variable name.
fieldNameToVariable :: FieldName -> G.Variable
fieldNameToVariable = G.Variable . G.Name . getFieldNameTxt

-- | Lookup the field in the schema.
lookupField ::
     G.Name
  -> ObjTyInfo
  -> Either (NonEmpty ValidationError) ObjFldInfo
lookupField name objTypeInfo =
  maybe (Left (pure (FieldNotFoundInType name objTypeInfo))) pure $
  HM.lookup name (_otiFields objTypeInfo)

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments ::
     ParamMap
  -> HM.HashMap G.Name G.Value
  -> HM.HashMap G.Variable (FieldInfo PGColumnInfo)
  -> TypeMap
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

-- | Validate a value against a type.
validateType ::
     HM.HashMap G.Variable (FieldInfo PGColumnInfo)
  -> G.Value
  -> G.GType
  -> TypeMap
  -> Validation (NonEmpty ValidationError) ()
validateType permittedVariables gvalue expectedGType types =
  case gvalue of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> Failure (pure (InvalidVariable variable permittedVariables))
        Just fieldInfo -> do
          bindValidation
            (fieldInfoToNamedType fieldInfo)
            (\actualNamedType ->
               assertType (G.toGT actualNamedType) expectedGType)
    G.VInt {} -> assertType (fromScalar PGInteger) expectedGType
    G.VFloat {} -> assertType (fromScalar PGFloat) expectedGType
    G.VBoolean {} -> assertType (fromScalar PGBoolean) expectedGType
    G.VNull -> Failure (pure NullNotAllowedHere)
    G.VString {} -> assertType (fromScalar PGText) expectedGType
    v@(G.VEnum _) -> Failure (pure (UnsupportedArgumentType v))
    G.VList (G.unListValue -> values) -> do
      (assertListType expectedGType)
      (flip
         traverse_
         values
         (\val ->
            validateType permittedVariables val (peelType expectedGType) types))
      pure ()
    G.VObject (G.unObjectValue -> values) ->
      flip
        traverse_
        values
        (\(G.ObjectFieldG name val) -> do
           let expectedNamedType = getBaseTy expectedGType
           case HM.lookup expectedNamedType types of
             Nothing ->
               Failure (pure $ TypeNotFoundInRemoteSchema expectedNamedType)
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
  where
   fromScalar  = G.toGT . mkScalarTy


assertType :: G.GType -> G.GType -> Validation (NonEmpty ValidationError) ()
assertType actualType expectedType =
  when
    (not $ checkEq actualType expectedType)
    (Failure (pure $ ExpectedTypeButGot expectedType actualType))
  where
    -- this check ignores nullability criterion
    checkEq type1 type2 = case (type1, type2) of
      (G.TypeNamed _ nt1, G.TypeNamed _ nt2 ) -> nt1 == nt2
      (G.TypeList _ lt1, G.TypeList _ lt2) -> checkEq (G.unListType lt1) (G.unListType lt2)
      _ -> False

assertListType :: G.GType -> Validation (NonEmpty ValidationError) ()
assertListType actualType =
  (when (not $ isListType actualType)
    (Failure (pure $ InvalidType actualType "is not a list type")))

-- | Convert a field info to a named type, if possible.
fieldInfoToNamedType ::
     (FieldInfo PGColumnInfo)
  -> Validation (NonEmpty ValidationError) G.NamedType
fieldInfoToNamedType =
  \case
    FIColumn colInfo -> case pgiType colInfo of
      PGColumnScalar scalarType -> pure $ mkScalarTy scalarType
      _                         -> Failure $ pure UnsupportedEnum
    FIRelationship relInfo ->
      Failure (pure (UnsupportedForeignRelationship relInfo))
    -- FIRemote remoteField ->
    --   Failure (pure (RemoteFieldsNotAllowedInArguments remoteField))

isListType :: G.GType -> Bool
isListType =
  \case
    G.TypeNamed {} -> False
    G.TypeList {} -> True

peelType :: G.GType -> G.GType
peelType =
  \case
    G.TypeList _ lt -> G.unListType lt
    nt -> nt

remoteArgumentsToMap :: RemoteArguments -> HM.HashMap G.Name G.Value
remoteArgumentsToMap =
  HM.fromList .
  map (\field -> (G._ofName field, G._ofValue field)) .
  getRemoteArguments
