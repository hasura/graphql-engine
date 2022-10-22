{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Responsible for translating and building an MSSQL execution plan for
--   delete mutations.
--
--   This module is used by "Hasura.Backends.MSSQL.Instances.Execute".
module Hasura.Backends.MSSQL.Execute.Delete
  ( executeDelete,
  )
where

import Database.MSSQL.Transaction qualified as Tx
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.QueryTags
import Hasura.Backends.MSSQL.FromIr as TSQL
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameDeleted)
import Hasura.Backends.MSSQL.FromIr.Delete qualified as TSQL
import Hasura.Backends.MSSQL.FromIr.MutationResponse
import Hasura.Backends.MSSQL.FromIr.SelectIntoTempTable qualified as TSQL
import Hasura.Backends.MSSQL.Plan
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.QueryTags (QueryTagsComment)
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Session

-- | Executes a Delete IR AST and return results as JSON.
executeDelete ::
  (MonadError QErr m, MonadReader QueryTagsComment m) =>
  UserInfo ->
  StringifyNumbers ->
  SourceConfig 'MSSQL ->
  AnnDelG 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (ExceptT QErr IO EncJSON)
executeDelete userInfo stringifyNum sourceConfig deleteOperation = do
  queryTags <- ask
  preparedDelete <- traverse (prepareValueQuery $ _uiSession userInfo) deleteOperation
  pure $ mssqlRunReadWrite (_mscExecCtx sourceConfig) (buildDeleteTx preparedDelete stringifyNum queryTags)

-- | Converts a Delete IR AST to a transaction of three delete sql statements.
--
-- A GraphQL delete mutation does two things:
--
-- 1. Deletes rows in a table according to some predicate
-- 2. (Potentially) returns the deleted rows (including relationships) as JSON
--
-- In order to complete these 2 things we need 3 SQL statements:
--
-- 1. @SELECT INTO <temp_table> WHERE <false>@ - creates a temporary table
--    with the same schema as the original table in which we'll store the deleted rows
--    from the table we are deleting
-- 2. @DELETE FROM with OUTPUT@ - deletes the rows from the table and inserts the
--   deleted rows to the temporary table from (1)
-- 3. @SELECT@ - constructs the @returning@ query from the temporary table, including
--   relationships with other tables.
buildDeleteTx ::
  AnnDel 'MSSQL ->
  StringifyNumbers ->
  QueryTagsComment ->
  Tx.TxET QErr IO EncJSON
buildDeleteTx deleteOperation stringifyNum queryTags = do
  let withAlias = "with_alias"
      createInsertedTempTableQuery =
        toQueryFlat $
          TQ.fromSelectIntoTempTable $
            TSQL.toSelectIntoTempTable tempTableNameDeleted (_adTable deleteOperation) (_adAllCols deleteOperation) RemoveConstraints
  -- Create a temp table
  Tx.unitQueryE defaultMSSQLTxErrorHandler (createInsertedTempTableQuery `withQueryTags` queryTags)
  let deleteQuery = TQ.fromDelete <$> TSQL.fromDelete deleteOperation
  deleteQueryValidated <- toQueryFlat <$> runFromIr deleteQuery
  -- Execute DELETE statement
  Tx.unitQueryE mutationMSSQLTxErrorHandler (deleteQueryValidated `withQueryTags` queryTags)
  mutationOutputSelect <- runFromIr $ mkMutationOutputSelect stringifyNum withAlias $ _adOutput deleteOperation

  let withSelect =
        emptySelect
          { selectProjections = [StarProjection],
            selectFrom = Just $ FromTempTable $ Aliased tempTableNameDeleted "deleted_alias"
          }
      finalMutationOutputSelect = mutationOutputSelect {selectWith = Just $ With $ pure $ Aliased withSelect withAlias}
      mutationOutputSelectQuery = toQueryFlat $ TQ.fromSelect finalMutationOutputSelect
  -- Execute SELECT query and fetch mutation response
  result <- encJFromText <$> Tx.singleRowQueryE defaultMSSQLTxErrorHandler (mutationOutputSelectQuery `withQueryTags` queryTags)
  -- delete the temporary table
  let dropDeletedTempTableQuery = toQueryFlat $ dropTempTableQuery tempTableNameDeleted
  Tx.unitQueryE defaultMSSQLTxErrorHandler (dropDeletedTempTableQuery `withQueryTags` queryTags)
  -- return results
  pure result
