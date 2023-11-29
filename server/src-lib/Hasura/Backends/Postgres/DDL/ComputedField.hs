-- | Postgres DDL ComputedField
--
-- How to build the 'ComputedFieldInfo' for a field.
--
-- See 'Hasura.RQL.Types.Metadata.Backend'.
module Hasura.Backends.Postgres.DDL.ComputedField
  ( buildComputedFieldInfo,
  )
where

import Control.Monad.Validate qualified as MV
import Data.HashSet qualified as S
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Backends.Postgres.DDL.Function
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Types.ComputedField qualified as PG
import Hasura.Backends.Postgres.Types.Function qualified as PG
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (Comment (..))
import Hasura.RQL.Types.ComputedField
import Hasura.Server.Utils
import Language.GraphQL.Draft.Syntax qualified as G

data ComputedFieldValidateError
  = CFVENotValidGraphQLName ComputedFieldName
  | CFVEInvalidTableArgument InvalidTableArgument
  | CFVEInvalidSessionArgument InvalidSessionArgument
  | CFVENotBaseReturnType PGScalarType
  | CFVEReturnTableNotFound QualifiedTable
  | CFVENoInputArguments
  | CFVEFunctionVolatile
  deriving (Show, Eq)

data InvalidTableArgument
  = ITANotFound FunctionArgName
  | ITANotComposite PG.FunctionTableArgument
  | ITANotTable QualifiedTable PG.FunctionTableArgument
  deriving (Show, Eq)

data InvalidSessionArgument
  = ISANotFound FunctionArgName
  | ISANotJSON PG.FunctionSessionArgument
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
    showFunctionTableArgument functionArg
      <> " of type "
      <> ty
      <<> " is not the table to which the computed field is being added"
  CFVEInvalidSessionArgument (ISANotFound argName) ->
    argName <<> " is not an input argument of the function " <>> qf
  CFVEInvalidSessionArgument (ISANotJSON functionArg) ->
    showFunctionSessionArgument functionArg <> " is not of type JSON"
  CFVENotBaseReturnType scalarType ->
    "the function "
      <> qf
      <<> " returning type "
      <> pgScalarTypeToText scalarType
      <> " is not a BASE type"
  CFVEReturnTableNotFound table ->
    "the function "
      <> qf
      <<> " returning set of table "
      <> table
      <<> " is not tracked or not found in database"
  CFVENoInputArguments ->
    "the function " <> qf <<> " has no input arguments"
  CFVEFunctionVolatile ->
    "the function " <> qf <<> " is of type VOLATILE; cannot be added as a computed field"
  where
    showFunctionTableArgument = \case
      PG.FTAFirst -> "first argument of the function " <>> qf
      PG.FTANamed argName _ -> argName <<> " argument of the function " <>> qf
    showFunctionSessionArgument = \case
      PG.FunctionSessionArgument argName _ -> argName <<> " argument of the function " <>> qf

buildComputedFieldInfo ::
  forall pgKind m.
  (QErrM m) =>
  -- | the set of all tracked tables
  S.HashSet QualifiedTable ->
  QualifiedTable ->
  S.HashSet PGCol ->
  ComputedFieldName ->
  PG.ComputedFieldDefinition ->
  PGRawFunctionInfo ->
  Comment ->
  m (ComputedFieldInfo ('Postgres pgKind))
buildComputedFieldInfo trackedTables table _tableColumns computedField definition rawFunctionInfo comment =
  either (throw400 NotSupported . showErrors) pure =<< MV.runValidateT mkComputedFieldInfo
  where
    inputArgNames = rfiInputArgNames rawFunctionInfo
    PG.ComputedFieldDefinition function maybeTableArg maybeSessionArg = definition
    functionReturnType =
      QualifiedPGType
        (rfiReturnTypeSchema rawFunctionInfo)
        (rfiReturnTypeName rawFunctionInfo)
        (rfiReturnTypeType rawFunctionInfo)

    computedFieldGraphQLName = G.mkName $ computedFieldNameToText computedField

    mkComputedFieldInfo ::
      (MV.MonadValidate [ComputedFieldValidateError] n) =>
      n (ComputedFieldInfo ('Postgres pgKind))
    mkComputedFieldInfo = do
      -- Check if computed field name is a valid GraphQL name
      unless (isJust computedFieldGraphQLName)
        $ MV.dispute
        $ pure
        $ CFVENotValidGraphQLName computedField

      -- Check if function is VOLATILE
      when (rfiFunctionType rawFunctionInfo == FTVOLATILE)
        $ MV.dispute
        $ pure CFVEFunctionVolatile

      -- Validate and resolve return type
      returnType <-
        if rfiReturnsTable rawFunctionInfo
          then do
            let returnTable = typeToTable functionReturnType
            unless (returnTable `S.member` trackedTables)
              $ MV.dispute
              $ pure
              $ CFVEReturnTableNotFound returnTable
            pure $ PG.CFRSetofTable returnTable
          else do
            let scalarType = _qptName functionReturnType
            unless (isBaseType functionReturnType)
              $ MV.dispute
              $ pure
              $ CFVENotBaseReturnType scalarType
            pure $ PG.CFRScalar scalarType

      -- Validate and resolve table argument
      let inputArgs =
            mkFunctionArgs
              (rfiDefaultArgs rawFunctionInfo)
              (rfiInputArgTypes rawFunctionInfo)
              inputArgNames
      tableArgument <- case maybeTableArg of
        Just argName ->
          case findWithIndex ((Just argName ==) . PG.faName) inputArgs of
            Just (tableArg, index) -> do
              let functionTableArg = PG.FTANamed argName index
              validateTableArgumentType functionTableArg $ PG.faType tableArg
              pure functionTableArg
            Nothing ->
              MV.refute $ pure $ CFVEInvalidTableArgument $ ITANotFound argName
        Nothing -> do
          case inputArgs of
            [] -> MV.dispute $ pure CFVENoInputArguments
            (firstArg : _) ->
              validateTableArgumentType PG.FTAFirst $ PG.faType firstArg
          pure PG.FTAFirst

      maybePGSessionArg <- sequence $ do
        argName <- maybeSessionArg
        return $ case findWithIndex ((Just argName ==) . PG.faName) inputArgs of
          Just (sessionArg, index) -> do
            let functionSessionArg = PG.FunctionSessionArgument argName index
            validateSessionArgumentType functionSessionArg $ PG.faType sessionArg
            pure functionSessionArg
          Nothing ->
            MV.refute $ pure $ CFVEInvalidSessionArgument $ ISANotFound argName

      let inputArgSeq =
            Seq.fromList
              $ dropTableAndSessionArgument tableArgument maybePGSessionArg inputArgs
          computedFieldArgs = PG.ComputedFieldImplicitArguments tableArgument maybePGSessionArg
          computedFieldFunction =
            ComputedFieldFunction function inputArgSeq computedFieldArgs $ rfiDescription rawFunctionInfo

      pure $ ComputedFieldInfo @('Postgres pgKind) () computedField computedFieldFunction returnType description

    validateTableArgumentType ::
      (MV.MonadValidate [ComputedFieldValidateError] n) =>
      PG.FunctionTableArgument ->
      QualifiedPGType ->
      n ()
    validateTableArgumentType tableArg qpt = do
      when (_qptType qpt /= PGKindComposite)
        $ MV.dispute
        $ pure
        $ CFVEInvalidTableArgument
        $ ITANotComposite tableArg
      let typeTable = typeToTable qpt
      unless (table == typeTable)
        $ MV.dispute
        $ pure
        $ CFVEInvalidTableArgument
        $ ITANotTable typeTable tableArg

    validateSessionArgumentType ::
      (MV.MonadValidate [ComputedFieldValidateError] n) =>
      PG.FunctionSessionArgument ->
      QualifiedPGType ->
      n ()
    validateSessionArgumentType sessionArg qpt = do
      unless (isJSONType $ _qptName qpt)
        $ MV.dispute
        $ pure
        $ CFVEInvalidSessionArgument
        $ ISANotJSON sessionArg

    showErrors :: [ComputedFieldValidateError] -> Text
    showErrors allErrors =
      "the computed field "
        <> computedField
        <<> " cannot be added to table "
        <> table
        <<> " "
        <> reasonMessage
      where
        reasonMessage = makeReasonMessage allErrors (showError function)

    dropTableAndSessionArgument ::
      PG.FunctionTableArgument ->
      Maybe PG.FunctionSessionArgument ->
      [PG.FunctionArg] ->
      [PG.FunctionArg]
    dropTableAndSessionArgument tableArg sessionArg inputArgs =
      let withoutTable = case tableArg of
            PG.FTAFirst -> tail inputArgs
            PG.FTANamed argName _ ->
              filter ((/=) (Just argName) . PG.faName) inputArgs
          alsoWithoutSession = case sessionArg of
            Nothing -> withoutTable
            Just (PG.FunctionSessionArgument name _) ->
              filter ((/=) (Just name) . PG.faName) withoutTable
       in alsoWithoutSession

    description :: Maybe Text
    description =
      case comment of
        Automatic -> commentFromDatabase <|> Just autogeneratedDescription
        Explicit value -> toTxt <$> value
      where
        commentFromDatabase = getPGDescription <$> rfiDescription rawFunctionInfo
        autogeneratedDescription =
          "A computed field, executes function " <>> function
