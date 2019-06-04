{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

-- | Validate input queries against remote schemas.

module Hasura.RQL.DDL.Remote.Validate
  ( getCreateRemoteRelationshipValidation
  , validateRelationship
  , validateRemoteArguments
  , ValidationError(..)
  ) where

import           Data.Bifunctor
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Validation
import qualified Hasura.GraphQL.Context as GC
import qualified Hasura.GraphQL.Schema as GS
import           Hasura.GraphQL.Validate.Types
import qualified Hasura.GraphQL.Validate.Types as VT
import           Hasura.Prelude
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.DDL.Remote.Input
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import qualified Language.GraphQL.Draft.Syntax as G

-- | An error validating the remote relationship.
data ValidationError
  = CouldntFindRemoteField G.Name VT.ObjTyInfo
  | FieldNotFoundInRemoteSchema G.Name
  | NoSuchArgumentForRemote G.Name
  | MissingRequiredArgument G.Name
  | TableNotFound !QualifiedTable
  | TableFieldNonexistent !QualifiedTable !FieldName
  | InvalidType
  | InvalidVariable G.Variable (HashMap G.Variable FieldInfo)
  | NullNotAllowedHere
  | ForeignRelationshipsNotAllowedInRemoteVariable !RelInfo
  deriving (Show, Eq)

-- | Get a validation for the remote relationship proposal.
getCreateRemoteRelationshipValidation ::
     (QErrM m, CacheRM m)
  => CreateRemoteRelationship
  -> m (Either (NonEmpty ValidationError) ())
getCreateRemoteRelationshipValidation createRemoteRelationship = do
  schemaCache <- askSchemaCache
  pure
    (validateRelationship
       createRemoteRelationship
       (scDefaultRemoteGCtx schemaCache)
       (scTables schemaCache))

-- | Validate a remote relationship given a context.
validateRelationship ::
     CreateRemoteRelationship
  -> GC.GCtx
  -> HashMap QualifiedTable TableInfo
  -> Either (NonEmpty ValidationError) ()
validateRelationship createRemoteRelationship gctx tables = do
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
             (toList (ccrHasuraFields createRemoteRelationship)))
      objFldInfo <-
        lookupField
          (ccrRemoteField createRemoteRelationship)
          (GS._gQueryRoot gctx)
      case VT._fiLoc objFldInfo of
        HasuraType ->
          Left
            (pure
               (FieldNotFoundInRemoteSchema
                  (ccrRemoteField createRemoteRelationship)))
        RemoteType {} ->
          toEither
            (validateRemoteArguments
               (VT._fiParams objFldInfo)
               (remoteArgumentsToMap
                  (ccrRemoteArguments createRemoteRelationship))
               (HM.fromList
                  (map (first fieldNameToVariable) (HM.toList fieldInfos))))
  where
    tableName = ccrTable createRemoteRelationship

-- | Convert a field name to a variable name.
fieldNameToVariable :: FieldName -> G.Variable
fieldNameToVariable = G.Variable . G.Name . getFieldNameTxt

-- | Lookup the field in the schema.
lookupField ::
     G.Name
  -> VT.ObjTyInfo
  -> Either (NonEmpty ValidationError) VT.ObjFldInfo
lookupField name objFldInfo = viaObject objFldInfo
  where
    viaObject =
      maybe (Left (pure (CouldntFindRemoteField name objFldInfo))) pure .
      HM.lookup name .
      VT._otiFields

-- | Validate remote input arguments against the remote schema.
validateRemoteArguments ::
     HashMap G.Name InpValInfo
  -> HashMap G.Name G.Value
  -> HashMap G.Variable FieldInfo
  -> Validation (NonEmpty ValidationError) ()
validateRemoteArguments expectedArguments providedArguments permittedVariables = do
  traverse validateProvided (HM.toList providedArguments)
  traverse validateExpected (HM.toList expectedArguments)
  pure ()
  where
    validateProvided (providedKey, providedValue) =
      case HM.lookup providedKey expectedArguments of
        Nothing -> Failure (pure (NoSuchArgumentForRemote providedKey))
        Just expectedType ->
          validateType permittedVariables providedValue (_iviType expectedType)
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
     HashMap G.Variable FieldInfo
  -> G.Value
  -> G.GType
  -> Validation (NonEmpty ValidationError) ()
validateType permittedVariables value (gtypeToEither -> expectedType) =
  bindValidation
    (valueType permittedVariables value)
    (\actualType ->
       when (actualType /= expectedType) (Failure (pure InvalidType)))

-- | Produce the type of a value.
valueType ::
     HashMap G.Variable FieldInfo
  -> G.Value
  -> Validation (NonEmpty ValidationError) (Either G.NamedType G.ListType)
valueType permittedVariables =
  \case
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> Failure (pure (InvalidVariable variable permittedVariables))
        Just fieldInfo -> fmap Left (fieldInfoToNamedType fieldInfo)
    G.VInt {} -> pure (Left (mkScalarTy PGInteger))
    G.VFloat {} -> pure (Left (mkScalarTy PGFloat))
    G.VBoolean {} -> pure (Left (mkScalarTy PGBoolean))
    G.VNull -> Failure (pure NullNotAllowedHere)
    G.VString {} -> pure (Left (mkScalarTy PGText))
    (G.VEnum _) -> error "TODO: G.VEnum: What do we do here?"
    (G.VList _) -> error "TODO: G.VList: What do we do here?"
    (G.VObject _) -> error "TODO: G.VObject: What do we do here?"

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
gtypeToEither :: G.GType -> Either G.NamedType G.ListType
gtypeToEither =
  \case
    G.TypeNamed _ namedType -> Left namedType
    G.TypeList _ listType -> Right listType
