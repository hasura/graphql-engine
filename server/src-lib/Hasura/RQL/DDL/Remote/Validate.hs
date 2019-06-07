{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.Remote.Validate
  ( getCreateRemoteRelationshipValidation
  , validateRelationship
  , validateRemoteArguments
  , ValidationError(..)
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Validation
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DDL.Remote.Input
import           Hasura.RQL.DDL.Remote.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict               as HM
import qualified Data.Text                         as T
import qualified Hasura.GraphQL.Context            as GC
import qualified Hasura.GraphQL.Schema             as GS
import qualified Language.GraphQL.Draft.Syntax     as G

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
  | UnsupportedArgumentType G.Value
  deriving (Show, Eq)

-- | Get a validation for the remote relationship proposal.
getCreateRemoteRelationshipValidation ::
     (QErrM m, CacheRM m)
  => RemoteRelationship
  -> m (Either (NonEmpty ValidationError) RemoteField)
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
  -> Either (NonEmpty ValidationError) RemoteField
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
      objFldInfo <-
        lookupField (rtrRemoteField remoteRelationship) (GS._gQueryRoot gctx)
      case _fiLoc objFldInfo of
        HasuraType ->
          Left
            (pure
               (FieldNotFoundInRemoteSchema (rtrRemoteField remoteRelationship)))
        RemoteType {} -> do
          toEither
            (validateRemoteArguments
               (_fiParams objFldInfo)
               (remoteArgumentsToMap (rtrRemoteArguments remoteRelationship))
               (HM.fromList
                  (map (first fieldNameToVariable) (HM.toList fieldInfos)))
               (GS._gTypes gctx))
          pure
            RemoteField
              { rmfRemoteRelationship = remoteRelationship
              , rmfGType = _fiTy objFldInfo
              }
  where
    tableName = rtrTable remoteRelationship

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
  traverse validateExpected (HM.toList expectedArguments)
  pure ()
  where
    validateProvided (providedName, providedValue) =
      case HM.lookup providedName expectedArguments of
        Nothing -> Failure (pure (NoSuchArgumentForRemote providedName))
        Just (_iviType -> expectedType) ->
          validateType permittedVariables providedValue expectedType types
    validateExpected (expectedKey, expectedInpValInfo) =
      if G.isNullable (_iviType expectedInpValInfo)
        then pure ()
        else case _iviDefVal expectedInpValInfo of
               Just {} -> pure ()
               Nothing ->
                 case HM.lookup expectedKey providedArguments of
                   Nothing ->
                     Failure (pure (MissingRequiredArgument expectedKey))
                   Just {} -> pure ()


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

-- | Reify the constructors to an Either.
isListType :: G.GType -> Bool
isListType =
  \case
    G.TypeNamed {} -> False
    G.TypeList {} -> True
