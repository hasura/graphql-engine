{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( validateRemoteRelationship
  , validateErrorToText
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Validation
--import           Hasura.GraphQL.Validate.Types
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Parser.Column
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
  | CouldntFindRemoteField !G.Name !G.Name
  | FieldNotFoundInRemoteSchema !G.Name
  | NoSuchArgumentForRemote !G.Name
  | MissingRequiredArgument !G.Name
  | TypeNotFound !G.Name
  | TableNotFound !QualifiedTable
  | TableFieldNonexistent !QualifiedTable !FieldName
  | ExpectedTypeButGot !G.GType !G.GType
  | InvalidType !G.GType !T.Text
  | InvalidVariable !G.Name !(HM.HashMap G.Name PGColumnInfo)
  | NullNotAllowedHere
  | InvalidGTypeForStripping !G.GType
  | UnsupportedMultipleElementLists
  | UnsupportedEnum
  | InvalidGraphQLName !T.Text
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

-- | Validate a remote relationship given a context.
validateRemoteRelationship
  :: (MonadError ValidationError m)
  => RemoteRelationship
  -> RemoteSchemaMap
  -> [PGColumnInfo]
  -> m RemoteFieldInfo
validateRemoteRelationship remoteRelationship remoteSchemaMap pgColumns = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
      table = rtrTable remoteRelationship
  hasuraFields <- forM (toList $ rtrHasuraFields remoteRelationship) $
    \fieldName -> case find ((==) fieldName . fromPGCol . pgiColumn) pgColumns of
                    Nothing -> throwError $ TableFieldNonexistent table fieldName
                    Just r  -> pure r
  case HM.lookup remoteSchemaName remoteSchemaMap of
    Nothing -> throwError $ RemoteSchemaNotFound remoteSchemaName
    Just (RemoteSchemaCtx rsName (schemaDoc,queryRootName,_,_) rsi) -> do
      queryRootFieldDefinitions <-
        case lookupObject schemaDoc queryRootName of
          Just obj ->
            pure $ G._otdFieldsDefinition obj
          Nothing -> throwError $ FieldNotFoundInRemoteSchema queryRootName
     -- (_, (leafParamMap, _)) <-
     --    foldlM
     --      (\(objTyInfo,_) fieldCall -> do
     --         objFldDefinition <- lookupField (fcName fieldCall) objTyInfo
     --         let providedArguments = getRemoteArguments $ fcArguments fieldCall
     --         (validateRemoteArguments
     --            (mapFromL G._fldName (G._fldArgumentsDefinition objFldInfo))
     --            providedArguments
     --            (HM.fromList
     --               (map
     --                  (first pgColumnToVariable)
     --                  (HM.toList $ mapFromL (pgiColumn) pgColumns)))
     --            (GS._gTypes gctx))                       -- I don't have this
             -- (newParamMap, newTypeMap) <-
             --   first
             --     pure
             --       (runStateT
             --          (stripInMap
             --             remoteRelationship
             --             (GS._gTypes gctx)
             --             (_fiParams objFldInfo)
             --             providedArguments)
             --          typeMap)
             -- innerObjTyInfo <-
             --   if isObjType (GS._gTypes gctx) objFldInfo
             --   then getTyInfoFromField (GS._gTypes gctx) objFldInfo
             --   else if isScalarType (GS._gTypes gctx) objFldInfo
             --          then pure objTyInfo
             --          else throwError $
             --                    (InvalidType (_fiTy objFldInfo) "only objects or scalar types expected")
--              pure
--                ( innerObjTyInfo
-- --             , _fiTy objFldInfo
--                , (newParamMap, newTypeMap)))
--                pure (queryRootFieldDefinitions,(mempty,mempty))
--           (pure
--              ( queryRootFieldDefinitions
-- --           , G.toGT (_otiName $ GS._gQueryRoot gctx)
--              , (mempty, mempty)))
--           (unRemoteFields $ rtrRemoteField remoteRelationship)
      pure $ RemoteFieldInfo
            { _rfiName = rtrName remoteRelationship
            , _rfiParamMap = _leafParamMap
            , _rfiHasuraFields = HS.fromList hasuraFields
            , _rfiRemoteFields = rtrRemoteField remoteRelationship
            , _rfiRemoteSchema = rsi
            , _rfiSchemaDoc = schemaDoc
            , _rfiRemoteSchemaName = rsName
            }
  -- where
  --   getTyInfoFromField types field =
  --     let baseTy = getBaseTy (_fiTy field)
  --         fieldName = _fiName field
  --         typeInfo = HM.lookup baseTy types
  --      in case typeInfo of
  --           Just (TIObj objTyInfo) -> pure objTyInfo
  --           _                      -> Left (pure (FieldNotFoundInRemoteSchema fieldName))
  --   isObjType types field =
  --     let baseTy = getBaseTy (_fiTy field)
  --         typeInfo = HM.lookup baseTy types
  --      in case typeInfo of
  --           Just (TIObj _) -> True
  --           _              -> False

  --   isScalarType types field =
  --     let baseTy = getBaseTy (_fiTy field)
  --         typeInfo = HM.lookup baseTy types
  --      in case typeInfo of
  --           Just (TIScalar _) -> True
  --           _                 -> False

    -- remoteArgumentsToMap =
    --   HM.fromList .
    --   map (\field -> (G._ofName field, G._ofValue field)) .
    --   getRemoteArguments

-- | Return a map with keys deleted whose template argument is
-- specified as an atomic (variable, constant), keys which are kept
-- have their values modified by 'stripObject' or 'stripList'.
-- stripInMap ::
--      RemoteRelationship -> HM.HashMap G.Name TypeInfo
--   -> HM.HashMap G.Name InpValInfo
--   -> HM.HashMap G.Name G.Value
--   -> StateT (HM.HashMap G.Name TypeInfo) (Either ValidationError) (HM.HashMap G.Name InpValInfo)
-- stripInMap remoteRelationshipName types schemaArguments templateArguments =
--   fmap
--     (HM.mapMaybe id)
--     (HM.traverseWithKey
--        (\name inpValInfo ->
--           case HM.lookup name templateArguments of
--             Nothing -> pure (Just inpValInfo)
--             Just value -> do
--               maybeNewGType <- stripValue remoteRelationshipName types (_iviType inpValInfo) value
--               pure
--                 (fmap
--                    (\newGType -> inpValInfo {_iviType = newGType})
--                    maybeNewGType))
--        schemaArguments)

-- | Strip a value type completely, or modify it, if the given value
-- is atomic-ish.
-- stripValue ::
--      RemoteRelationship -> HM.HashMap G.Name TypeInfo
--   -> G.GType
--   -> G.Value
--   -> StateT (HM.HashMap G.Name TypeInfo) (Either ValidationError) (Maybe G.GType)
-- stripValue remoteRelationshipName types gtype value = do
--   case value of
--     G.VVariable {} -> pure Nothing
--     G.VInt {} -> pure Nothing
--     G.VFloat {} -> pure Nothing
--     G.VString {} -> pure Nothing
--     G.VBoolean {} -> pure Nothing
--     G.VNull {} -> pure Nothing
--     G.VEnum {} -> pure Nothing
--     G.VList (G.ListValueG values) ->
--       case values of
--         []       -> pure Nothing
--         [gvalue] -> stripList remoteRelationshipName types gtype gvalue
--         _        -> lift (Left UnsupportedMultipleElementLists)
--     G.VObject (G.unObjectValue -> keypairs) ->
--       fmap Just (stripObject remoteRelationshipName types gtype keypairs)

-- -- | Produce a new type for the list, or strip it entirely.
-- stripList ::
--      RemoteRelationship
--   -> HM.HashMap G.Name TypeInfo
--   -> G.GType
--   -> G.Value
--   -> StateT (HM.HashMap G.Name TypeInfo) (Either ValidationError) (Maybe G.GType)
-- stripList remoteRelationshipName types originalOuterGType value =
--   case originalOuterGType of
--     G.TypeList nullability (G.ListType innerGType) -> do
--       maybeNewInnerGType <- stripValue remoteRelationshipName types innerGType value
--       pure
--         (fmap
--            (\newGType -> G.TypeList nullability (G.ListType newGType))
--            maybeNewInnerGType)
--     _ -> lift (Left (InvalidGTypeForStripping originalOuterGType))

-- -- | Produce a new type for the given InpValInfo, modified by
-- -- 'stripInMap'. Objects can't be deleted entirely, just keys of an
-- -- object.
-- stripObject ::
--      RemoteRelationship -> HM.HashMap G.Name TypeInfo
--   -> G.GType
--   -> [G.ObjectFieldG G.Value]
--   -> StateT (HM.HashMap G.Name TypeInfo) (Either ValidationError) G.GType
-- stripObject remoteRelationshipName types originalGtype keypairs =
--   case originalGtype of
--     G.TypeNamed nullability originalNamedType ->
--       case HM.lookup (getBaseTy originalGtype) types of
--         Just (TIInpObj originalInpObjTyInfo) -> do
--           let originalSchemaArguments = _iotiFields originalInpObjTyInfo
--               newNamedType =
--                 renameNamedType
--                   (renameTypeForRelationship remoteRelationshipName)
--                   originalNamedType
--           newSchemaArguments <-
--             stripInMap
--               remoteRelationshipName
--               types
--               originalSchemaArguments
--               templateArguments
--           let newInpObjTyInfo =
--                 originalInpObjTyInfo
--                   {_iotiFields = newSchemaArguments, _iotiName = newNamedType}
--               newGtype = G.TypeNamed nullability newNamedType
--           modify (HM.insert newNamedType (TIInpObj newInpObjTyInfo))
--           pure newGtype
--         _ -> lift (Left (InvalidGTypeForStripping originalGtype))
--     _ -> lift (Left (InvalidGTypeForStripping originalGtype))
--   where
--     templateArguments :: HM.HashMap G.Name G.Value
--     templateArguments =
--       HM.fromList (map (\(G.ObjectFieldG key val) -> (key, val)) keypairs)

-- -- | Produce a new name for a type, used when stripping the schema
-- -- types for a remote relationship.
-- -- TODO: Consider a separator character to avoid conflicts.
-- renameTypeForRelationship :: RemoteRelationship -> Text -> Text
-- renameTypeForRelationship rtr text =
--   text <> "_remote_rel_" <> name
--   where name = schema <> "_" <> table <> remoteRelationshipNameToText (rtrName rtr)
--         QualifiedObject (SchemaName schema) (TableName table) = rtrTable rtr

-- -- | Rename a type.
-- renameNamedType :: (Text -> Text) -> G.Name -> G.Name
-- renameNamedType rename (G.Name (G.Name text)) =
--   G.Name (G.Name (rename text))

-- -- | Convert a field name to a variable name.
-- pgColumnToVariable :: PGCol -> G.Variable
-- pgColumnToVariable = G.Variable . G.Name . getPGColTxt

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
-- validateRemoteArguments ::
--      HM.HashMap G.Name InpValInfo
--   -> HM.HashMap G.Name G.Value
--   -> HM.HashMap G.Variable PGColumnInfo
--   -> HM.HashMap G.Name TypeInfo
--   -> Validation (NonEmpty ValidationError) ()
-- validateRemoteArguments expectedArguments providedArguments permittedVariables types = do
--   traverse validateProvided (HM.toList providedArguments)
--   -- Not neccessary to validate if all required args are provided in the relationship
--   -- traverse validateExpected (HM.toList expectedArguments)
--   pure ()
--   where
--     validateProvided (providedName, providedValue) =
--       case HM.lookup providedName expectedArguments of
--         Nothing -> Failure (pure (NoSuchArgumentForRemote providedName))
--         Just (_iviType -> expectedType) ->
--           validateType permittedVariables providedValue expectedType types

-- | Validate a value against a type.
-- validateType ::
--      HM.HashMap G.Variable PGColumnInfo
--   -> G.Value
--   -> G.GType
--   -> HM.HashMap G.Name TypeInfo
--   -> Validation (NonEmpty ValidationError) ()
-- validateType permittedVariables value expectedGType types =
--   case value of
--     G.VVariable variable ->
--       case HM.lookup variable permittedVariables of
--         Nothing -> Failure (pure (InvalidVariable variable permittedVariables))
--         Just fieldInfo ->
--           bindValidation
--             (columnInfoToNamedType fieldInfo)
--             (\actualNamedType -> assertType (G.toGT actualNamedType) expectedGType)
--     G.VInt {} -> assertType (G.toGT $ mkScalarTy PGInteger) expectedGType
--     G.VFloat {} -> assertType (G.toGT $ mkScalarTy PGFloat) expectedGType
--     G.VBoolean {} -> assertType (G.toGT $ mkScalarTy PGBoolean) expectedGType
--     G.VNull -> Failure (pure NullNotAllowedHere)
--     G.VString {} -> assertType (G.toGT $ mkScalarTy PGText) expectedGType
--     G.VEnum _ -> Failure (pure UnsupportedEnum)
--     G.VList (G.unListValue -> values) -> do
--       case values of
--         []  -> pure ()
--         [_] -> pure ()
--         _   -> Failure (pure UnsupportedMultipleElementLists)
--       (assertListType expectedGType)
--       (flip
--          traverse_
--          values
--          (\val ->
--             validateType permittedVariables val (unwrapTy expectedGType) types))
--       pure ()
--     G.VObject (G.unObjectValue -> values) ->
--       flip
--         traverse_
--         values
--         (\(G.ObjectFieldG name val) ->
--            let expectedNamedType = getBaseTy expectedGType
--            in
--            case HM.lookup expectedNamedType types of
--              Nothing -> Failure (pure $ TypeNotFound expectedNamedType)
--              Just typeInfo ->
--                case typeInfo of
--                  TIInpObj inpObjTypeInfo ->
--                    case HM.lookup name (_iotiFields inpObjTypeInfo) of
--                      Nothing -> Failure (pure $ NoSuchArgumentForRemote name)
--                      Just (_iviType -> expectedType) ->
--                        validateType permittedVariables val expectedType types
--                  _ ->
--                    Failure
--                      (pure $
--                       InvalidType
--                         (G.toGT $ G.Name name)
--                         "not an input object type"))

assertType
  :: (MonadError ValidationError m)
  => G.GType
  -> G.GType
  -> m ()
assertType actualType expectedType = do
  -- check if both are list types or both are named types
  (when
     (G.isListType actualType /= G.isListType expectedType)
     (throwError $ ExpectedTypeButGot expectedType actualType))
  -- if list type then check over unwrapped type, else check base types
  if G.isListType actualType
    then assertType (unwrapType actualType) (unwrapType expectedType)
    else (when
            (G.getBaseType actualType /= G.getBaseType expectedType)
            (throwError $ ExpectedTypeButGot expectedType actualType))
  pure ()
  where
    unwrapType = \case
      G.TypeList _ lt -> lt
      nt -> nt

assertListType :: (MonadError ValidationError m) => G.GType -> m ()
assertListType actualType =
  (when (not $ G.isListType actualType)
    (throwError $ InvalidType actualType "is not a list type"))

-- | Convert a field info to a named type, if possible.
columnInfoToNamedType
  :: (MonadError ValidationError m)
  => PGColumnInfo
  -> m G.Name
columnInfoToNamedType pci =
  case pgiType pci of
    PGColumnScalar scalarType -> do
      eitherScalar <- runExceptT $ mkScalarTypeName scalarType
      case eitherScalar of
        Left _ -> throwError $ InvalidGraphQLName $ toSQLTxt scalarType
        Right s -> pure s
    _                         -> throwError UnsupportedEnum
