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
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.DDL.Remote.Input
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
  | ExpectedTypeButGot !G.NamedType !G.NamedType
  | InvalidType !G.NamedType !T.Text
  | InvalidVariable G.Variable (HM.HashMap G.Variable FieldInfo)
  | NullNotAllowedHere
  | ForeignRelationshipsNotAllowedInRemoteVariable !RelInfo
  | UnsupportedArgumentType G.Value
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
  -> HM.HashMap QualifiedTable TableInfo
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
      case _fiLoc objFldInfo of
        HasuraType ->
          Left
            (pure
               (FieldNotFoundInRemoteSchema
                  (ccrRemoteField createRemoteRelationship)))
        RemoteType {} ->
          toEither
            (validateRemoteArguments
               (_fiParams objFldInfo)
               (remoteArgumentsToMap
                  (ccrRemoteArguments createRemoteRelationship))
               (HM.fromList
                  (map (first fieldNameToVariable) (HM.toList fieldInfos)))
               (GS._gTypes gctx)
            )
  where
    tableName = ccrTable createRemoteRelationship

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
validateType permittedVariables value gType@(getBaseTy -> expectedNamedType) types =
  case value of
    G.VVariable variable ->
      case HM.lookup variable permittedVariables of
        Nothing -> Failure (pure (InvalidVariable variable permittedVariables))
        Just fieldInfo ->
          bindValidation
            (fieldInfoToNamedType fieldInfo)
            (flip assertType expectedNamedType)
    G.VInt {} -> assertType (mkScalarTy PGInteger) expectedNamedType
    G.VFloat {} -> assertType (mkScalarTy PGFloat) expectedNamedType
    G.VBoolean {} -> assertType (mkScalarTy PGBoolean) expectedNamedType
    G.VNull -> Failure (pure NullNotAllowedHere)
    G.VString {} -> assertType (mkScalarTy PGText) expectedNamedType
    v@(G.VEnum _) -> Failure (pure (UnsupportedArgumentType v))
    G.VList (G.unListValue -> values) ->
      flip
        traverse_
        values
        (\val -> validateType permittedVariables val gType types)
    G.VObject (G.unObjectValue -> values) ->
      flip
        traverse_
        values
        (\(G.ObjectFieldG name val) ->
           case HM.lookup expectedNamedType types of
             Nothing -> Failure (pure $ TypeNotFound expectedNamedType)
             Just typeInfo ->
               case typeInfo of
                 TIInpObj inpObjTypeInfo ->
                   case HM.lookup name (_iotiFields inpObjTypeInfo) of
                     Nothing -> Failure (pure $ NoSuchArgumentForRemote name)
                     Just (_iviType -> expectedType) ->
                       validateType permittedVariables val expectedType types
                 _ -> Failure (pure $ InvalidType expectedNamedType "not an input object type"))

assertType ::
     G.NamedType -> G.NamedType -> Validation (NonEmpty ValidationError) ()
assertType actualType expectedType =
  (when (actualType /= expectedType)
    (Failure (pure $ ExpectedTypeButGot expectedType actualType)))

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
