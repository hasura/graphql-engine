{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Responsible for translating and building an MSSQL execution plan for
--   update mutations.
--
--   This module is used by "Hasura.Backends.MSSQL.Instances.Execute".
module Hasura.Backends.MSSQL.Execute.Update
  ( executeUpdate,
  )
where

import Database.MSSQL.Transaction qualified as Tx
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.QueryTags
import Hasura.Backends.MSSQL.FromIr as TSQL
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameUpdated)
import Hasura.Backends.MSSQL.FromIr.Expression (fromGBoolExp)
import Hasura.Backends.MSSQL.FromIr.MutationResponse
import Hasura.Backends.MSSQL.FromIr.SelectIntoTempTable qualified as TSQL
import Hasura.Backends.MSSQL.FromIr.Update qualified as TSQL
import Hasura.Backends.MSSQL.Plan
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.Prelude
import Hasura.QueryTags (QueryTagsComment)
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.IR.ModelInformation.Types (ModelNameInfo (..))
import Hasura.RQL.IR.Update.Batch qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (SourceName (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Session

-- | Executes an Update IR AST and return results as JSON.
executeUpdate ::
  (MonadError QErr m, MonadReader QueryTagsComment m) =>
  UserInfo ->
  Options.StringifyNumbers ->
  SourceName ->
  ModelSourceType ->
  SourceConfig 'MSSQL ->
  AnnotatedUpdateG 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (OnBaseMonad (ExceptT QErr) EncJSON, [ModelNameInfo])
executeUpdate userInfo stringifyNum sourceName modelSourceType sourceConfig updateOperation = do
  queryTags <- ask
  let mssqlExecCtx = (_mscExecCtx sourceConfig)
  preparedUpdate <- traverse (prepareValueQuery $ _uiSession userInfo) updateOperation

  let (modelName, modelType) = (tableName (_auTable updateOperation), ModelTypeTable)
  let returnModels = getMutationOutputModelNamesGen sourceName modelSourceType (_auOutput updateOperation)
  let mutationUpdateVariant = IR._ubWhere $ _auUpdateVariant updateOperation
  argModelNames <- do
    (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType $ mutationUpdateVariant
    pure res
  preUpdatePermissionModelNames <- do
    (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType $ _auUpdatePermissions updateOperation
    pure res
  postUpdateCheckModelNames <- do
    (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType $ _auCheck updateOperation
    pure res
  let modelNames = [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)] <> preUpdatePermissionModelNames <> postUpdateCheckModelNames <> (argModelNames) <> (returnModels)
  if IR.updateBatchIsEmpty $ _auUpdateVariant updateOperation
    then pure $ (OnBaseMonad $ pure $ IR.buildEmptyMutResp $ _auOutput preparedUpdate, modelNames)
    else pure $ (OnBaseMonad $ (mssqlRunReadWrite mssqlExecCtx) (buildUpdateTx preparedUpdate stringifyNum queryTags), modelNames)

-- | Converts an Update IR AST to a transaction of three update sql statements.
--
-- A GraphQL update mutation does two things:
--
-- 1. Update rows in a table according to some predicate
-- 2. (Potentially) returns the updated rows (including relationships) as JSON
--
-- In order to complete these 2 things we need 3 SQL statements:
--
-- 1. @SELECT INTO <temp_table> WHERE <false>@ - creates a temporary table
--    with the same schema as the original table in which we'll store the updated rows
--    from the table we are deleting
-- 2. @UPDATE SET FROM with OUTPUT@ - updates the rows from the table and inserts the
--   updated rows to the temporary table from (1)
-- 3. @SELECT@ - constructs the @returning@ query from the temporary table, including
--   relationships with other tables.
buildUpdateTx ::
  (MonadIO m) =>
  AnnotatedUpdate 'MSSQL ->
  Options.StringifyNumbers ->
  QueryTagsComment ->
  Tx.TxET QErr m EncJSON
buildUpdateTx updateOperation stringifyNum queryTags = do
  let withAlias = "with_alias"
      createInsertedTempTableQuery =
        toQueryFlat
          $ TQ.fromSelectIntoTempTable
          $ TSQL.toSelectIntoTempTable tempTableNameUpdated (_auTable updateOperation) (_auAllCols updateOperation) RemoveConstraints
  -- Create a temp table
  Tx.unitQueryE defaultMSSQLTxErrorHandler (createInsertedTempTableQuery `withQueryTags` queryTags)
  let updateQuery = TQ.fromUpdate <$> TSQL.fromUpdate updateOperation
  updateQueryValidated <- toQueryFlat . qwdQuery <$> runFromIrErrorOnCTEs updateQuery

  -- Execute UPDATE statement
  Tx.unitQueryE mutationMSSQLTxErrorHandler (updateQueryValidated `withQueryTags` queryTags)
  mutationOutputSelect <- qwdQuery <$> runFromIrUseCTEs (mkMutationOutputSelect stringifyNum withAlias $ _auOutput updateOperation)
  let checkCondition = _auCheck updateOperation

  -- The check constraint is translated to boolean expression
  checkBoolExp <- qwdQuery <$> runFromIrErrorOnCTEs (runReaderT (fromGBoolExp checkCondition) (EntityAlias withAlias))

  let withSelect =
        emptySelect
          { selectProjections = [StarProjection],
            selectFrom = Just $ FromTempTable $ Aliased tempTableNameUpdated "updated_alias"
          }
      mutationOutputCheckConstraintSelect = selectMutationOutputAndCheckCondition withAlias mutationOutputSelect checkBoolExp
      finalSelect = mutationOutputCheckConstraintSelect {selectWith = Just $ With $ pure $ Aliased (CTESelect withSelect) withAlias}

  -- Execute SELECT query to fetch mutation response and check constraint result
  let finalSelectQuery = toQueryFlat $ TQ.fromSelect finalSelect
  (responseText, checkConditionInt) <- Tx.singleRowQueryE defaultMSSQLTxErrorHandler (finalSelectQuery `withQueryTags` queryTags)
  -- Drop the temp table
  Tx.unitQueryE defaultMSSQLTxErrorHandler (toQueryFlat (dropTempTableQuery tempTableNameUpdated) `withQueryTags` queryTags)
  -- Raise an exception if the check condition is not met
  unless (checkConditionInt == (0 :: Int))
    $ throw400 PermissionError "check constraint of an insert/update permission has failed"
  pure $ encJFromText responseText
