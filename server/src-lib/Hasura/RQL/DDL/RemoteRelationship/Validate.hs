{-# LANGUAGE ViewPatterns #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.RemoteRelationship.Validate
  ( validateRemoteRelationship
  , errorToText
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Validation
--import           Hasura.GraphQL.Validate.Types
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Schema
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
  pgColumnsVariables <- (mapM (\(k,v) -> do
                                  variableName <- pgColumnToVariable k
                                  pure $ (variableName,v)
                              )) $ (HM.toList $ mapFromL (pgiColumn) pgColumns)
  let pgColumnsVariablesMap = HM.fromList pgColumnsVariables
  case HM.lookup remoteSchemaName remoteSchemaMap of
    Nothing -> throwError $ RemoteSchemaNotFound remoteSchemaName
    Just (RemoteSchemaCtx rsName (schemaDoc,queryRootName,_,_) rsi) -> do
      queryRoot <-
        case lookupObject schemaDoc queryRootName of
          Just obj -> pure obj
          _ -> throwError $ FieldNotFoundInRemoteSchema queryRootName
      _ <-
        foldlM
          (\objTyInfo fieldCall -> do
              objFldDefinition <- lookupField (fcName fieldCall) objTyInfo
              let providedArguments = getRemoteArguments $ fcArguments fieldCall
              (validateRemoteArguments
                (mapFromL G._ivdName (G._fldArgumentsDefinition objFldDefinition))
                providedArguments
                pgColumnsVariablesMap
                schemaDoc)
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
              innerObjTyInfo <- do
                case getObjTyInfoFromField schemaDoc objFldDefinition of
                  Just obj -> pure obj
                  Nothing ->
                    bool (throwError $
                                (InvalidType (G._fldType objFldDefinition) "only objects or scalar types expected"))
                         (pure objTyInfo)
                         (isScalarType schemaDoc objFldDefinition)
--              pure
--                ( innerObjTyInfo
-- --             , _fiTy objFldInfo
--                , (newParamMap, newTypeMap)))
              pure innerObjTyInfo) -- temporary
          queryRoot
          (unRemoteFields $ rtrRemoteField remoteRelationship)
      pure $ RemoteFieldInfo
            { _rfiName = rtrName remoteRelationship
            , _rfiParamMap = mempty -- _leafParamMap
            , _rfiHasuraFields = HS.fromList hasuraFields
            , _rfiRemoteFields = rtrRemoteField remoteRelationship
            , _rfiRemoteSchema = rsi
            , _rfiSchemaDoc = schemaDoc
            , _rfiRemoteSchemaName = rsName
            }
  where
    getObjTyInfoFromField schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
          fieldName = _fiName field
      in lookupObject schemaDoc baseTy

    isScalarType schemaDoc field =
      let baseTy = G.getBaseType (G._fldType field)
      in isJust $ lookupScalar schemaDoc baseTy

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

-- | Convert a field name to a variable name.
pgColumnToVariable :: (MonadError ValidationError m) => PGCol -> m G.Name
pgColumnToVariable pgCol =
  let pgColText = getPGColTxt pgCol
  in maybe (throwError $ InvalidGraphQLName pgColText) pure $ G.mkName pgColText

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
  -> HM.HashMap G.Name PGColumnInfo
  -> G.SchemaDocument
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
  nt -> nt

-- | Validate a value against a type.
validateType
  :: (MonadError ValidationError m)
  => HM.HashMap G.Name PGColumnInfo
  -> G.Value G.Name
  -> G.GType
  -> G.SchemaDocument
  -> m ()
validateType permittedVariables value expectedGType schemaDocument =
  case value of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> throwError (InvalidVariable variable permittedVariables)
        Just fieldInfo -> do
          namedType <- columnInfoToNamedType fieldInfo
          assertType (mkGraphQLType namedType) expectedGType
    G.VInt {} -> do
      intScalarGType <- (mkGraphQLType <$> mkScalarTy PGInteger)
      assertType intScalarGType expectedGType
    G.VFloat {} -> do
      floatScalarGType <- (mkGraphQLType <$> mkScalarTy PGFloat)
      assertType floatScalarGType expectedGType
    G.VBoolean {} -> do
      boolScalarGType <- (mkGraphQLType <$> mkScalarTy PGBoolean)
      assertType boolScalarGType expectedGType
    G.VNull -> throwError NullNotAllowedHere
    G.VString {} -> do
      stringScalarGType <- (mkGraphQLType <$> mkScalarTy PGText)
      assertType stringScalarGType expectedGType
    G.VEnum _ -> throwError UnsupportedEnum
    G.VList values -> do
      case values of
        []  -> pure ()
        [_] -> pure ()
        _   -> throwError UnsupportedMultipleElementLists
      assertListType expectedGType
      (flip
         traverse_
         values
         (\val ->
            validateType permittedVariables val (unwrapGraphQLType expectedGType) schemaDocument))
    G.VObject values ->
      flip
        traverse_
        (HM.toList values)
        (\(name,val) ->
           let expectedNamedType = G.getBaseType expectedGType
           in
           case lookupType schemaDocument expectedNamedType of
             Nothing -> throwError $ (TypeNotFound expectedNamedType)
             Just typeInfo ->
               case typeInfo of
                 G.TypeDefinitionInputObject inpObjTypeInfo ->
                   let objectTypeDefnsMap =
                         mapFromL G._ivdName $ (G._iotdValueDefinitions inpObjTypeInfo)
                   in
                   case HM.lookup name objectTypeDefnsMap of
                     Nothing -> throwError $ NoSuchArgumentForRemote name
                     Just (G._ivdType -> expectedType) ->
                       validateType permittedVariables val expectedType schemaDocument
                 _ -> do
                   throwError $ InvalidType (mkGraphQLType name) "not an input object type")
  where
    mkGraphQLType =
      G.TypeNamed (G.Nullability True)

    mkScalarTy scalarType = do
      eitherScalar <- runExceptT $ mkScalarTypeName scalarType
      case eitherScalar of
        Left _ -> throwError $ InvalidGraphQLName $ toSQLTxt scalarType
        Right s -> pure s

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
    then assertType (unwrapGraphQLType actualType) (unwrapGraphQLType expectedType)
    else (when
            (G.getBaseType actualType /= G.getBaseType expectedType)
            (throwError $ ExpectedTypeButGot expectedType actualType))
  pure ()

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
