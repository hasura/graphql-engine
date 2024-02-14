-- | BigQuery DDL ComputedField
--
--  Implementation to build 'ComputedFieldInfo' for a BigQuery table from metadata
module Hasura.Backends.BigQuery.DDL.ComputedField
  ( buildComputedFieldInfo,
  )
where

import Control.Monad.Validate qualified as MV
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Backends.BigQuery.DDL.Source
import Hasura.Backends.BigQuery.Instances.Types ()
import Hasura.Backends.BigQuery.Meta
import Hasura.Backends.BigQuery.Types
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.Server.Utils
import Language.GraphQL.Draft.Syntax qualified as G

-- | Errors that occur when validating and building computed fields.
data ComputedFieldError
  = CFENoArgumentType FunctionArgName
  | CFENotTableValuedFunction
  | CFENoInputArguments
  | CFENoArgumentName
  | CFEInvalidArgumentName FunctionArgName
  | CFEInvalidColumnName TableName ColumnName
  | CFEReturnTableNotTracked TableName
  | CFENeedReturnTableName
  | CFENotRelevantReturnTable TableName
  | CFEReturnTableSchemaError ReturnTableSchemaError

-- | Errors that occur when validating returning table schema fields
data ReturnTableSchemaError
  = RTSENoFieldName
  | RTSENoType Text
  | RTSENotValidGraphQLName Text

-- | Generate read-able error message
showError :: FunctionName -> ComputedFieldError -> Text
showError functionName = \case
  CFENoArgumentType argName ->
    "argument " <> argName <<> " has no data type defined in the function " <>> functionName
  CFENotTableValuedFunction ->
    prefixFunction <> " is not a TABLE_VALUED_FUNCTION"
  CFENoInputArguments ->
    prefixFunction <> " has no input arguments defined"
  CFENoArgumentName ->
    prefixFunction <> " has at least one argument without name"
  CFEInvalidArgumentName argName ->
    "the argument " <> argName <<> " is not one of function " <> functionName <<> " input arguments"
  CFEInvalidColumnName tableName columnName ->
    "the column " <> columnName <<> " does not exist in table " <>> tableName
  CFEReturnTableNotTracked tableName ->
    prefixFunction <> " returning set of table " <> tableName <<> " is not tracked"
  CFENeedReturnTableName ->
    prefixFunction <> " is not defined with 'RETURNS TABLE'. Expecting return table name."
  CFENotRelevantReturnTable tableName ->
    "return table " <> tableName <<> " is not required as the function " <> functionName <<> " returns arbitrary column fields"
  CFEReturnTableSchemaError returnFieldsError -> showReturnFieldsError returnFieldsError
  where
    showReturnFieldsError :: ReturnTableSchemaError -> Text
    showReturnFieldsError = \case
      RTSENoFieldName -> "at least one field name is absent"
      RTSENoType fieldName -> "fieldName " <> fieldName <<> " has not type information"
      RTSENotValidGraphQLName fieldName -> "fieldName " <> fieldName <<> " is not a valid GraphQL name"

    prefixFunction :: Text
    prefixFunction = "function " <>> functionName

-- | Validate computed field metadata and build field information
buildComputedFieldInfo ::
  forall m.
  (MonadError QErr m) =>
  HashSet TableName ->
  TableName ->
  HashSet ColumnName ->
  ComputedFieldName ->
  ComputedFieldDefinition ->
  RestRoutine ->
  Comment ->
  m (ComputedFieldInfo 'BigQuery)
buildComputedFieldInfo trackedTables table tableColumns computedField ComputedFieldDefinition {..} restRoutine comment = do
  either (throw400 NotSupported . showErrors) pure =<< MV.runValidateT mkComputedFieldInfo
  where
    mkComputedFieldInfo ::
      forall n.
      (MV.MonadValidate [ComputedFieldError] n) =>
      n (ComputedFieldInfo 'BigQuery)
    mkComputedFieldInfo = do
      -- Currently, we only support functions returning set of rows in computed field.
      -- Support for scalar computed fields is being tracked at https://github.com/hasura/graphql-engine/issues/8521.
      unless (routineType restRoutine == TABLE_VALUED_FUNCTION) $ MV.dispute $ pure CFENotTableValuedFunction
      restArguments <- onNothing (arguments restRoutine) $ MV.refute (pure CFENoInputArguments)
      inputArguments <- Seq.fromList <$> for restArguments resolveInputArgument
      for_ (HashMap.toList _bqcfdArgumentMapping) (validateArgumentMapping inputArguments)
      let fieldFunction = ComputedFieldFunction _bqcfdFunction inputArguments _bqcfdArgumentMapping Nothing
      fieldReturn <- resolveFunctionReturning (returnTableType restRoutine) _bqcfdReturnTable
      pure $ ComputedFieldInfo @'BigQuery () computedField fieldFunction fieldReturn $ commentToMaybeText comment
      where
        resolveInputArgument :: RestArgument -> n FunctionArgument
        resolveInputArgument RestArgument {..} = do
          case _raName of
            Nothing -> MV.refute $ pure CFENoArgumentName
            Just name -> do
              let argName = FunctionArgName name
              restType <- onNothing _raDataType $ MV.refute $ pure $ CFENoArgumentType argName
              pure $ FunctionArgument argName (restTypeToScalarType restType)

        validateArgumentMapping :: Seq FunctionArgument -> (FunctionArgName, ColumnName) -> n ()
        validateArgumentMapping args (functionArgName, columnName) = do
          -- Check if argument is one of function input arguments
          unless (functionArgName `elem` (_faName <$> args)) $ MV.dispute $ pure $ CFEInvalidArgumentName functionArgName
          -- Check if column name exist in list of table columns
          unless (columnName `HS.member` tableColumns) $ MV.dispute $ pure $ CFEInvalidColumnName table columnName

        resolveReturnSqlFields :: [RestStandardSqlField] -> n [(ColumnName, G.Name, ScalarType)]
        resolveReturnSqlFields fields =
          forM fields $ \RestStandardSqlField {..} -> do
            fieldName <- onNothing _rssfName $ MV.refute $ pure $ CFEReturnTableSchemaError RTSENoFieldName
            fieldGraphQLName <- onNothing (G.mkName fieldName) $ MV.refute $ pure $ CFEReturnTableSchemaError $ RTSENotValidGraphQLName fieldName
            type' <- onNothing _rssType $ MV.refute $ pure $ CFEReturnTableSchemaError $ RTSENoType fieldName
            pure (ColumnName fieldName, fieldGraphQLName, restTypeToScalarType type')

        resolveFunctionReturning ::
          Maybe RestStandardSqlTableType ->
          Maybe TableName ->
          n ComputedFieldReturn
        resolveFunctionReturning Nothing Nothing =
          MV.refute $ pure CFENeedReturnTableName
        resolveFunctionReturning Nothing (Just returnTable) =
          -- Function does not return schema of a table. The return table type should be inferred
          -- from function definition. The user provides returning table name through metadata.
          -- Check if returning table is tracked.
          if returnTable `HS.member` trackedTables
            then pure (ReturnExistingTable returnTable)
            else MV.refute $ pure $ CFEReturnTableNotTracked returnTable
        resolveFunctionReturning (Just returnFields) Nothing =
          -- Return table is not specified and the function returns a schema of a table,
          -- specified as a list of column names and their type.
          ReturnTableSchema <$> resolveReturnSqlFields (_rrttColumns returnFields)
        resolveFunctionReturning (Just _) (Just returnTable) =
          MV.refute $ pure $ CFENotRelevantReturnTable returnTable

    showErrors :: [ComputedFieldError] -> Text
    showErrors allErrors =
      "the computed field "
        <> computedField
        <<> " cannot be added to table "
        <> table
        <<> " "
        <> reasonMessage
      where
        reasonMessage = makeReasonMessage allErrors (showError _bqcfdFunction)
