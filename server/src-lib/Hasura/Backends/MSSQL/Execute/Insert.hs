{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Responsible for translating and building an MSSQL execution plan for
--   delete mutations.
--
--   This module is used by "Hasura.Backends.MSSQL.Instances.Execute".
module Hasura.Backends.MSSQL.Execute.Insert
  ( executeInsert,
  )
where

import Control.Monad.Validate qualified as V
import Database.MSSQL.Transaction qualified as Tx
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.MutationResponse
import Hasura.Backends.MSSQL.FromIr as TSQL
import Hasura.Backends.MSSQL.Plan
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Backends.MSSQL.Types.Insert (BackendInsert (..), IfMatched)
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Parser
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.Session

-- | Execute and insert/upsert mutation against MS SQL Server.
--   See the documentation for 'buildInsertTx' to see how it's done.
executeInsert ::
  MonadError QErr m =>
  UserInfo ->
  Bool ->
  SourceConfig 'MSSQL ->
  AnnInsert 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (ExceptT QErr IO EncJSON)
executeInsert userInfo stringifyNum sourceConfig annInsert = do
  -- Convert the leaf values from @'UnpreparedValue' to sql @'Expression'
  insert <- traverse (prepareValueQuery sessionVariables) annInsert
  let insertTx = buildInsertTx tableName withAlias stringifyNum insert
  pure $ mssqlRunReadWrite (_mscExecCtx sourceConfig) insertTx
  where
    sessionVariables = _uiSession userInfo
    tableName = _aiTableName $ _aiData annInsert
    withAlias = "with_alias"

-- | Translates an IR Insert/upsert mutation description to SQL and
-- builds a corresponding transaction to run against MS SQL Server.
--
-- Execution of a MSSQL insert mutation broadly involves two steps.
--
-- > insert_table(objects: [
-- >   {column1: value1, column2: value2},
-- >   {column1: value3, column2: value4}
-- >  ],
-- >  if_matched: {match_columns: [column1], update_columns: [column2]} # Optional field to enable upserting
-- > ){
-- >   affected_rows
-- >   returning {
-- >     column1
-- >     column2
-- >   }
-- > }
--
-- = Step 1: Inserting rows into the table
--
-- a. Create an empty temporary table with name #inserted to store affected rows (for the response)
--
--    > SELECT column1, column2 INTO #inserted FROM some_table WHERE (1 <> 1)
--    > UNION ALL SELECT column1, column2 FROM some_table WHERE (1 <> 1);
--
-- b. If 'if_matched' is found: Use MERGE statment to perform upsert
--
--       b.1 Use #values temporary table to store input object values
--
--          > SELECT column1, column2 INTO #values FROM some_table WHERE (1 <> 1)
--
--       b.2 Insert input object values into the temporary table
--
--          > INSERT INTO #values (column1, column2) VALUES (value1, value2), (value3, value4)
--
--
--       b.3 Generate an SQL Merge statement to perform either update or insert (upsert) to the table
--
--           > MERGE some_table AS [target]
--           > USING (SELECT column1, column2 from #values) AS [source](column1, column2) ON ([target].column1 = [source].column1)
--           > WHEN MATCHED THEN UPDATE SET [column2] = [source].[column2]
--           > WHEN NOT MATCHED THEN INSERT (column1, column2) VALUES ([source].column1, [source].column2)
--           > OUTPUT INSERTED.column1, INSERTED.column2 INTO #inserted(column1, column2)
--
--    __NOTE__: In @MERGE@ statement, we use @SELECT query from a temporary table@ as source but not @VALUES@ expression
--          because, we can't use @DEFAULT@ expression (for missing columns in @objects@ field) in @VALUES@ expression.
--
--    __else__: Generate an SQL Insert statement from the GraphQL insert mutation with OUTPUT expression to fill @#inserted@ temporary table with inserted rows
--
--       > INSERT INTO some_table (column1, column2) OUTPUT INSERTED.column1, INSERTED.column2 INTO #inserted(column1, column2) VALUES (value1, value2), (value3, value4);
--
-- = Step 2: Generation of the mutation response
--
--    An SQL statement is generated and when executed it returns the mutation selection set containing 'affected_rows' and 'returning' field values.
--    The statement is generated with multiple sub select queries explained below:
--
-- a. A SQL Select statement to fetch only inserted rows from temporary table
--
--    > <table_select> := SELECT * FROM #inserted
--
--    The above select statement is referred through a common table expression - @WITH [with_alias] AS (<table_select>)@
--
-- b. The @affected_rows@ field value is obtained by using @COUNT@ aggregation and the @returning@ field selection set is translated to
--    a SQL select statement using 'mkSQLSelect'.
--
--    > <mutation_output_select> :=
--    >   SELECT (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows], (select_from_returning) AS [returning]
--    >   FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--
-- c. Evaluate the check constraint using @CASE@ expression. We use @SUM@ aggregation to check if any inserted row has failed the check constraint.
--
--   > <check_constraint_select> :=
--   >   SELECT SUM(CASE WHEN <check_boolean_expression> THEN 0 ELSE 1 END)
--   >   FROM [with_alias]
--
-- d. The final select statement look like
--
--    > WITH "with_alias" AS (<table_select>)
--    > SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
--
--    When executed, the above statement returns a single row with mutation response as a string value and check constraint result as an integer value.
buildInsertTx :: TSQL.TableName -> Text -> Bool -> AnnInsert 'MSSQL Void Expression -> Tx.TxET QErr IO EncJSON
buildInsertTx tableName withAlias stringifyNum insert = do
  let tableColumns = _aiTableCols $ _aiData insert
      ifMatchedField = _biIfMatched . _aiBackendInsert . _aiData $ insert

  -- Create #inserted temporary table
  let createInsertedTempTableQuery =
        toQueryFlat $
          TQ.fromSelectIntoTempTable $
            TSQL.toSelectIntoTempTable tempTableNameInserted tableName tableColumns RemoveConstraints

  Tx.unitQueryE fromMSSQLTxError createInsertedTempTableQuery

  -- Choose between running a regular @INSERT INTO@ statement or a @MERGE@ statement
  -- depending on the @if_matched@ field.
  --
  -- Affected rows will be inserted into the #inserted temporary table regardless.
  case ifMatchedField of
    Nothing -> do
      -- Insert values into the table using INSERT query
      let insertQuery = toQueryFlat $ TQ.fromInsert $ TSQL.fromInsert insert
      Tx.unitQueryE fromMSSQLTxError insertQuery
    Just ifMatched -> buildUpsertTx tableName insert ifMatched

  -- Build a response to the user using the values in the temporary table named #inserted
  (responseText, checkConditionInt) <- buildInsertResponseTx stringifyNum withAlias insert

  -- Drop the #inserted temp table
  Tx.unitQueryE fromMSSQLTxError $ toQueryFlat $ dropTempTableQuery tempTableNameInserted

  -- Raise an exception if the check condition is not met
  unless (checkConditionInt == 0) $
    throw400 PermissionError "check constraint of an insert permission has failed"

  pure $ encJFromText responseText

-- | Translates an IR IfMatched clause to SQL and
--   builds a corresponding transaction to run against MS SQL Server.
--
--   We do this in 2 steps:
--
--   1. Create a temporary table called @#values@ which will hold the values the user want to insert,
--      and insert the values into it
--   2. Build an run a @MERGE@ statement to either insert or upsert the values from the temporary table @#values@
--      into the original table, and output the affected rows into another temporary table called @#inserted@
--      which will be used to build a "response" for the user.
--
--   Should be used as part of a bigger transaction in 'buildInsertTx'.
buildUpsertTx :: TSQL.TableName -> AnnInsert 'MSSQL Void Expression -> IfMatched Expression -> Tx.TxET QErr IO ()
buildUpsertTx tableName insert ifMatched = do
  let insertColumnNames = concatMap (map fst . getInsertColumns) $ _aiInsObj $ _aiData insert
      allTableColumns = _aiTableCols $ _aiData insert
      insertColumns = filter (\c -> ciColumn c `elem` insertColumnNames) allTableColumns
      createValuesTempTableQuery =
        toQueryFlat $
          TQ.fromSelectIntoTempTable $
            -- We want to KeepConstraints here so the user can omit values for identity columns such as `id`
            TSQL.toSelectIntoTempTable tempTableNameValues tableName insertColumns KeepConstraints
  -- Create #values temporary table
  Tx.unitQueryE fromMSSQLTxError createValuesTempTableQuery

  -- Store values in #values temporary table
  let insertValuesIntoTempTableQuery =
        toQueryFlat $
          TQ.fromInsertValuesIntoTempTable $
            TSQL.toInsertValuesIntoTempTable tempTableNameValues insert
  Tx.unitQueryE fromMSSQLTxError insertValuesIntoTempTableQuery

  -- Run the MERGE query and store the mutated rows in #inserted temporary table
  merge <-
    (V.runValidate . runFromIr)
      (toMerge tableName (_aiInsObj $ _aiData insert) allTableColumns ifMatched)
      `onLeft` (throw500 . tshow)
  let mergeQuery = toQueryFlat $ TQ.fromMerge merge
  Tx.unitQueryE fromMSSQLTxError mergeQuery

  -- After @MERGE@ we no longer need this temporary table
  Tx.unitQueryE fromMSSQLTxError $ toQueryFlat $ dropTempTableQuery tempTableNameValues

-- | Builds a response to the user using the values in the temporary table named #inserted.
buildInsertResponseTx :: Bool -> Text -> AnnInsert 'MSSQL Void Expression -> Tx.TxET QErr IO (Text, Int)
buildInsertResponseTx stringifyNum withAlias insert = do
  -- Generate a SQL SELECT statement which outputs the mutation response using the #inserted
  mutationOutputSelect <- mkMutationOutputSelect stringifyNum withAlias $ _aiOutput insert

  -- The check constraint is translated to boolean expression
  let checkCondition = fst $ _aiCheckCond $ _aiData insert
  checkBoolExp <-
    V.runValidate (runFromIr $ runReaderT (fromGBoolExp checkCondition) (EntityAlias withAlias))
      `onLeft` (throw500 . tshow)

  let withSelect =
        emptySelect
          { selectProjections = [StarProjection],
            selectFrom = Just $ FromTempTable $ Aliased tempTableNameInserted "inserted_alias"
          }
      -- SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
      mutationOutputCheckConstraintSelect = selectMutationOutputAndCheckCondition withAlias mutationOutputSelect checkBoolExp
      -- WITH "with_alias" AS (<table_select>)
      -- SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
      finalSelect = mutationOutputCheckConstraintSelect {selectWith = Just $ With $ pure $ Aliased withSelect withAlias}

  -- Execute SELECT query to fetch mutation response and check constraint result
  Tx.singleRowQueryE fromMSSQLTxError (toQueryFlat $ TQ.fromSelect finalSelect)
