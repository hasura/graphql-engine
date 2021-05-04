module Hasura.Backends.Postgres.DDL.Field
  ( buildComputedFieldInfo
  , buildRemoteFieldInfo
  )
where

import           Hasura.Prelude

import qualified Control.Monad.Validate                     as MV
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashSet                               as S
import qualified Data.Sequence                              as Seq
import qualified Language.GraphQL.Draft.Syntax              as G

import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend                      as AB

import           Hasura.Backends.Postgres.DDL.Function
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Utils


data ComputedFieldValidateError
  = CFVENotValidGraphQLName !ComputedFieldName
  | CFVEInvalidTableArgument !InvalidTableArgument
  | CFVEInvalidSessionArgument !InvalidSessionArgument
  | CFVENotBaseReturnType !PGScalarType
  | CFVEReturnTableNotFound !QualifiedTable
  | CFVENoInputArguments
  | CFVEFunctionVolatile
  deriving (Show, Eq)

data InvalidTableArgument
  = ITANotFound !FunctionArgName
  | ITANotComposite !FunctionTableArgument
  | ITANotTable !QualifiedTable !FunctionTableArgument
  deriving (Show, Eq)

data InvalidSessionArgument
  = ISANotFound !FunctionArgName
  | ISANotJSON !FunctionSessionArgument
  deriving (Show, Eq)

showError :: QualifiedFunction -> ComputedFieldValidateError -> Text
showError qf = \case
  CFVENotValidGraphQLName computedField ->
    computedField <<> " is not valid GraphQL name"
  CFVEInvalidTableArgument (ITANotFound argName) ->
    argName <<> " is not an input argument of the function " <>> qf
  CFVEInvalidTableArgument (ITANotComposite functionArg) ->
    showFunctionTableArgument functionArg <> " is not COMPOSITE type"
  CFVEInvalidTableArgument (ITANotTable ty functionArg) ->
    showFunctionTableArgument functionArg <> " of type " <> ty
    <<> " is not the table to which the computed field is being added"
  CFVEInvalidSessionArgument (ISANotFound argName) ->
    argName <<> " is not an input argument of the function " <>> qf
  CFVEInvalidSessionArgument (ISANotJSON functionArg) ->
    showFunctionSessionArgument functionArg <> " is not of type JSON"
  CFVENotBaseReturnType scalarType ->
    "the function " <> qf <<> " returning type " <> toSQLTxt scalarType
    <> " is not a BASE type"
  CFVEReturnTableNotFound table ->
    "the function " <> qf <<> " returning set of table " <> table
    <<> " is not tracked or not found in database"
  CFVENoInputArguments ->
    "the function " <> qf <<> " has no input arguments"
  CFVEFunctionVolatile ->
    "the function " <> qf <<> " is of type VOLATILE; cannot be added as a computed field"
  where
    showFunctionTableArgument = \case
      FTAFirst           -> "first argument of the function " <>> qf
      FTANamed argName _ -> argName <<> " argument of the function " <>> qf
    showFunctionSessionArgument = \case
      FunctionSessionArgument argName _ -> argName <<> " argument of the function " <>> qf

buildComputedFieldInfo
  :: forall pgKind m. (QErrM m)
  => S.HashSet QualifiedTable
  -- ^ the set of all tracked tables
  -> QualifiedTable
  -> ComputedFieldName
  -> ComputedFieldDefinition ('Postgres pgKind)
  -> RawFunctionInfo
  -> Maybe Text
  -> m (ComputedFieldInfo ('Postgres pgKind))
buildComputedFieldInfo trackedTables table computedField definition rawFunctionInfo comment =
  either (throw400 NotSupported . showErrors) pure =<< MV.runValidateT mkComputedFieldInfo
  where
    inputArgNames = rfiInputArgNames rawFunctionInfo
    ComputedFieldDefinition function maybeTableArg maybeSessionArg = definition
    functionReturnType = QualifiedPGType (rfiReturnTypeSchema rawFunctionInfo)
                         (rfiReturnTypeName rawFunctionInfo)
                         (rfiReturnTypeType rawFunctionInfo)

    computedFieldGraphQLName = G.mkName $ computedFieldNameToText computedField

    mkComputedFieldInfo
      :: MV.MonadValidate [ComputedFieldValidateError] n
      => n (ComputedFieldInfo ('Postgres pgKind))
    mkComputedFieldInfo = do
      -- Check if computed field name is a valid GraphQL name
      unless (isJust computedFieldGraphQLName) $
        MV.dispute $ pure $ CFVENotValidGraphQLName computedField

      -- Check if function is VOLATILE
      when (rfiFunctionType rawFunctionInfo == FTVOLATILE) $
        MV.dispute $ pure CFVEFunctionVolatile

      -- Validate and resolve return type
      returnType <-
        if rfiReturnsTable rawFunctionInfo then do
          let returnTable = typeToTable functionReturnType
          unless (returnTable `S.member` trackedTables) $ MV.dispute $ pure $
            CFVEReturnTableNotFound returnTable
          pure $ CFRSetofTable returnTable
        else do
          let scalarType = _qptName functionReturnType
          unless (isBaseType functionReturnType) $ MV.dispute $ pure $
            CFVENotBaseReturnType scalarType
          pure $ CFRScalar scalarType

      -- Validate and resolve table argument
      let inputArgs = mkFunctionArgs
            (rfiDefaultArgs   rawFunctionInfo)
            (rfiInputArgTypes rawFunctionInfo)
            inputArgNames
      tableArgument <- case maybeTableArg of
        Just argName ->
          case findWithIndex ((Just argName ==) . faName) inputArgs of
            Just (tableArg, index) -> do
              let functionTableArg = FTANamed argName index
              validateTableArgumentType functionTableArg $ faType tableArg
              pure functionTableArg
            Nothing ->
              MV.refute $ pure $ CFVEInvalidTableArgument $ ITANotFound argName
        Nothing -> do
          case inputArgs of
            []           -> MV.dispute $ pure CFVENoInputArguments
            (firstArg:_) ->
              validateTableArgumentType FTAFirst $ faType firstArg
          pure FTAFirst

      maybePGSessionArg <- sequence $ do
          argName <- maybeSessionArg
          return $ case findWithIndex ((Just argName ==) . faName) inputArgs of
            Just (sessionArg, index) -> do
              let functionSessionArg = FunctionSessionArgument argName index
              validateSessionArgumentType functionSessionArg $ faType sessionArg
              pure functionSessionArg
            Nothing ->
              MV.refute $ pure $ CFVEInvalidSessionArgument $ ISANotFound argName


      let inputArgSeq = Seq.fromList $
            dropTableAndSessionArgument tableArgument maybePGSessionArg inputArgs
          computedFieldFunction =
            ComputedFieldFunction function inputArgSeq tableArgument maybePGSessionArg $
            rfiDescription rawFunctionInfo

      pure $ ComputedFieldInfo @('Postgres pgKind) () computedField computedFieldFunction returnType comment

    validateTableArgumentType
      :: (MV.MonadValidate [ComputedFieldValidateError] n)
      => FunctionTableArgument
      -> QualifiedPGType
      -> n ()
    validateTableArgumentType tableArg qpt = do
      when (_qptType qpt /= PGKindComposite) $
        MV.dispute $ pure $ CFVEInvalidTableArgument $ ITANotComposite tableArg
      let typeTable = typeToTable qpt
      unless (table == typeTable) $
        MV.dispute $ pure $ CFVEInvalidTableArgument $ ITANotTable typeTable tableArg

    validateSessionArgumentType
      :: (MV.MonadValidate [ComputedFieldValidateError] n)
      => FunctionSessionArgument
      -> QualifiedPGType
      -> n ()
    validateSessionArgumentType sessionArg qpt = do
      unless (isJSONType $ _qptName qpt) $
        MV.dispute $ pure $ CFVEInvalidSessionArgument $ ISANotJSON sessionArg

    showErrors :: [ComputedFieldValidateError] -> Text
    showErrors allErrors =
      "the computed field " <> computedField <<> " cannot be added to table "
      <> table <<> " " <> reasonMessage
      where
        reasonMessage = makeReasonMessage allErrors (showError function)

    dropTableAndSessionArgument :: FunctionTableArgument
                                -> Maybe FunctionSessionArgument -> [FunctionArg ('Postgres pgKind)]
                                -> [FunctionArg ('Postgres pgKind)]
    dropTableAndSessionArgument tableArg sessionArg inputArgs =
      let withoutTable = case tableArg of
            FTAFirst  -> tail inputArgs
            FTANamed argName _ ->
              filter ((/=) (Just argName) . faName) inputArgs
          alsoWithoutSession = case sessionArg of
            Nothing -> withoutTable
            Just (FunctionSessionArgument name _) ->
              filter ((/=) (Just name) . faName) withoutTable
      in alsoWithoutSession

buildRemoteFieldInfo
  :: forall m pgKind
   . (Backend ('Postgres pgKind), QErrM m)
  => RemoteRelationship ('Postgres pgKind)
  -> [ColumnInfo ('Postgres pgKind)]
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo ('Postgres pgKind), [SchemaDependency])
buildRemoteFieldInfo remoteRelationship
                          pgColumns
                          remoteSchemaMap = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
  (RemoteSchemaCtx _name introspectionResult remoteSchemaInfo _ _ _permissions) <-
    onNothing (Map.lookup remoteSchemaName remoteSchemaMap)
      $ throw400 RemoteSchemaError $ "remote schema with name " <> remoteSchemaName <<> " not found"
  eitherRemoteField <- runExceptT $
    validateRemoteRelationship remoteRelationship (remoteSchemaInfo, introspectionResult) pgColumns
  remoteField <- onLeft eitherRemoteField $ throw400 RemoteSchemaError . errorToText
  let table = rtrTable remoteRelationship
      source = rtrSource remoteRelationship
      schemaDependencies =
        let tableDep = SchemaDependency
                         (SOSourceObj source
                            $ AB.mkAnyBackend
                            $ SOITable @('Postgres pgKind) table)
                         DRTable
            columnsDep =
              map
                (flip SchemaDependency DRRemoteRelationship
                   . SOSourceObj source
                   . AB.mkAnyBackend
                   . SOITableObj @('Postgres pgKind)table
                   . TOCol @('Postgres pgKind)
                   . pgiColumn)
                $ S.toList $ _rfiHasuraFields remoteField
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema remoteSchemaName) DRRemoteSchema
         in (tableDep : remoteSchemaDep : columnsDep)

  pure (remoteField, schemaDependencies)
