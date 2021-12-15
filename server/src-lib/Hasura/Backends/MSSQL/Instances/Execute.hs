{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Execute
  ( MultiplexedQuery' (..),
    multiplexRootReselect,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Validate qualified as V
import Data.Aeson.Extended qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended qualified as T
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.Internal qualified as ODBCI
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.FromIr as TSQL
import Hasura.Backends.MSSQL.Plan
import Hasura.Backends.MSSQL.SQL.Value (txtEncodedColVal)
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Backends.MSSQL.Types.Insert (BackendInsert (..), ExtraColumnInfo (..))
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Backends.MSSQL.Types.Update
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.LiveQuery.Plan
import Hasura.GraphQL.Namespace (RootFieldAlias (..), RootFieldMap)
import Hasura.GraphQL.Parser
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types
import Hasura.RQL.Types qualified as RQLTypes
import Hasura.RQL.Types.Column qualified as RQLColumn
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

instance BackendExecute 'MSSQL where
  type PreparedQuery 'MSSQL = Text
  type MultiplexedQuery 'MSSQL = MultiplexedQuery'
  type ExecutionMonad 'MSSQL = ExceptT QErr IO

  mkDBQueryPlan = msDBQueryPlan
  mkDBMutationPlan = msDBMutationPlan
  mkDBSubscriptionPlan = msDBSubscriptionPlan
  mkDBQueryExplain = msDBQueryExplain
  mkLiveQueryExplain = msDBLiveQueryExplain

  -- NOTE: Currently unimplemented!.
  --
  -- This function is just a stub for future implementation; for now it just
  -- throws a 500 error.
  mkDBRemoteRelationshipPlan =
    msDBRemoteRelationshipPlan

-- Multiplexed query

newtype MultiplexedQuery' = MultiplexedQuery' Reselect

instance T.ToTxt MultiplexedQuery' where
  toTxt (MultiplexedQuery' reselect) = T.toTxt $ toQueryPretty $ fromReselect reselect

-- Query

msDBQueryPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (DBStepInfo 'MSSQL)
msDBQueryPlan userInfo sourceName sourceConfig qrf = do
  -- TODO (naveen): Append Query Tags to the query
  let sessionVariables = _uiSession userInfo
  statement <- planQuery sessionVariables qrf
  let printer = fromSelect statement
      queryString = ODBC.renderQuery $ toQueryPretty printer
      pool = _mscConnectionPool sourceConfig
      odbcQuery = encJFromText <$> runJSONPathQuery pool (toQueryFlat printer)
  pure $ DBStepInfo @'MSSQL sourceName sourceConfig (Just queryString) odbcQuery

runShowplan ::
  ODBC.Query -> ODBC.Connection -> IO [Text]
runShowplan query conn = do
  ODBC.exec conn "SET SHOWPLAN_TEXT ON"
  texts <- ODBC.query conn query
  ODBC.exec conn "SET SHOWPLAN_TEXT OFF"
  -- we don't need to use 'finally' here - if an exception occurs,
  -- the connection is removed from the resource pool in 'withResource'.
  pure texts

msDBQueryExplain ::
  MonadError QErr m =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (AB.AnyBackend DBStepInfo)
msDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  let sessionVariables = _uiSession userInfo
  statement <- planQuery sessionVariables qrf
  let query = toQueryPretty (fromSelect statement)
      queryString = ODBC.renderQuery query
      pool = _mscConnectionPool sourceConfig
      odbcQuery =
        withMSSQLPool
          pool
          ( \conn -> liftIO do
              showplan <- runShowplan query conn
              pure
                ( encJFromJValue $
                    ExplainPlan
                      fieldName
                      (Just queryString)
                      (Just showplan)
                )
          )
  pure $
    AB.mkAnyBackend $
      DBStepInfo @'MSSQL sourceName sourceConfig Nothing odbcQuery

msDBLiveQueryExplain ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  LiveQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL) ->
  m LiveQueryPlanExplanation
msDBLiveQueryExplain (LiveQueryPlan plan sourceConfig variables _) = do
  let (MultiplexedQuery' reselect) = _plqpQuery plan
      query = toQueryPretty $ fromSelect $ multiplexRootReselect [(dummyCohortId, variables)] reselect
      pool = _mscConnectionPool sourceConfig
  explainInfo <- withMSSQLPool pool (liftIO . runShowplan query)
  pure $ LiveQueryPlanExplanation (T.toTxt query) explainInfo variables

--------------------------------------------------------------------------------
-- Producing the correct SQL-level list comprehension to multiplex a query

-- Problem description:
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--

multiplexRootReselect ::
  [(CohortId, CohortVariables)] ->
  TSQL.Reselect ->
  TSQL.Select
multiplexRootReselect variables rootReselect =
  emptySelect
    { selectTop = NoTop,
      selectProjections =
        [ FieldNameProjection
            Aliased
              { aliasedThing =
                  TSQL.FieldName
                    { fieldNameEntity = rowAlias,
                      fieldName = resultIdAlias
                    },
                aliasedAlias = resultIdAlias
              },
          ExpressionProjection
            Aliased
              { aliasedThing =
                  ColumnExpression
                    ( TSQL.FieldName
                        { fieldNameEntity = resultAlias,
                          fieldName = TSQL.jsonFieldName
                        }
                    ),
                aliasedAlias = resultAlias
              }
        ],
      selectFrom =
        Just $
          FromOpenJson
            Aliased
              { aliasedThing =
                  OpenJson
                    { openJsonExpression =
                        ValueExpression (ODBC.TextValue $ lbsToTxt $ J.encode variables),
                      openJsonWith =
                        Just $
                          NE.fromList
                            [ UuidField resultIdAlias (Just $ IndexPath RootPath 0),
                              JsonField resultVarsAlias (Just $ IndexPath RootPath 1)
                            ]
                    },
                aliasedAlias = rowAlias
              },
      selectJoins =
        [ Join
            { joinSource = JoinReselect rootReselect,
              joinJoinAlias =
                JoinAlias
                  { joinAliasEntity = resultAlias,
                    joinAliasField = Just TSQL.jsonFieldName
                  }
            }
        ],
      selectWhere = Where mempty,
      selectFor =
        JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
      selectOrderBy = Nothing,
      selectOffset = Nothing
    }

-- mutation

msDBMutationPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  Bool ->
  SourceName ->
  SourceConfig 'MSSQL ->
  MutationDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (DBStepInfo 'MSSQL)
msDBMutationPlan userInfo stringifyNum sourceName sourceConfig mrf = do
  go <$> case mrf of
    MDBInsert annInsert -> executeInsert userInfo stringifyNum sourceConfig annInsert
    MDBDelete annDelete -> executeDelete userInfo stringifyNum sourceConfig annDelete
    MDBUpdate annUpdate -> executeUpdate userInfo stringifyNum sourceConfig annUpdate
    MDBFunction {} -> throw400 NotSupported "function mutations are not supported in MSSQL"
  where
    go v = DBStepInfo @'MSSQL sourceName sourceConfig Nothing v

-- | Execution of a MSSQL insert mutation broadly involves two steps.
--
-- --    insert_table(objects: [
-- --      {column1: value1, column2: value2},
-- --      {column1: value3, column2: value4}
-- --     ]
-- --    ){
-- --      affected_rows
-- --      returning {
-- --        column1
-- --        column2
-- --      }
-- --    }
-- --
-- Step 1: Inserting rows into the table
-- --
-- -- a. Generate an SQL Insert statement from the GraphQL insert mutation with OUTPUT expression to return
-- --    primary key column values after insertion.
-- -- b. Before insert, Set IDENTITY_INSERT to ON if any insert row contains atleast one identity column.
-- --
-- --    SET IDENTITY_INSERT some_table ON;
-- --    INSERT INTO some_table (column1, column2) OUTPUT INSERTED.pkey_column1, INSERTED.pkey_column2 VALUES (value1, value2), (value3, value4);
-- --
-- Step 2: Generation of the mutation response
-- --
-- --    An SQL statement is generated and when executed it returns the mutation selection set containing 'affected_rows' and 'returning' field values.
-- --    The statement is generated with multiple sub select queries explained below:
-- --
-- -- a. A SQL Select statement to fetch only inserted rows from the table using primary key column values fetched from
-- --    Step 1 in the WHERE clause
-- --
-- --    <table_select> :=
-- --      SELECT * FROM some_table WHERE (pkey_column1 = value1 AND pkey_column2 = value2) OR (pkey_column1 = value3 AND pkey_column2 = value4)
-- --
-- --    The above select statement is referred through a common table expression - "WITH [with_alias] AS (<table_select>)"
-- --
-- -- b. The 'affected_rows' field value is obtained by using COUNT aggregation and the 'returning' field selection set is translated to
-- --    a SQL select statement using @'mkSQLSelect'.
-- --
-- --    <mutation_output_select> :=
-- --      SELECT (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows], (select_from_returning) AS [returning] FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
-- --
-- -- c. Evaluate the check constraint using CASE expression. We use SUM aggregation to check if any inserted row has failed the check constraint.
-- --
-- --   <check_constraint_select> :=
-- --     SELECT SUM(CASE WHEN <check_boolean_expression> THEN 0 ELSE 1 END) FROM [with_alias]
-- --
-- -- d. The final select statement look like
-- --
-- --    WITH "with_alias" AS (<table_select>)
-- --    SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
-- --
-- --    When executed, the above statement returns a single row with mutation response as a string value and check constraint result as an integer value.
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
  let insertTx = buildInsertTx insert
  pure $ withMSSQLPool pool $ Tx.runTxE fromMSSQLTxError insertTx
  where
    sessionVariables = _uiSession userInfo
    pool = _mscConnectionPool sourceConfig
    table = _aiTableName $ _aiData annInsert
    withSelectTableAlias = "t_" <> tableName table
    withAlias = "with_alias"

    buildInsertTx :: AnnInsert 'MSSQL Void Expression -> Tx.TxET QErr IO EncJSON
    buildInsertTx insert = do
      let identityColumns = _eciIdentityColumns $ _biExtraColumnInfo $ _aiBackendInsert $ _aiData insert
          insertColumns = concatMap (map fst . getInsertColumns) $ _aiInsObj $ _aiData insert

      -- Set identity insert to ON if insert object contains identity columns
      when (any (`elem` identityColumns) insertColumns) $
        Tx.unitQueryE fromMSSQLTxError $
          toQueryFlat $
            TQ.fromSetIdentityInsert $
              SetIdentityInsert (_aiTableName $ _aiData insert) SetON

      -- Generate the INSERT query
      let insertQuery = toQueryFlat $ TQ.fromInsert $ TSQL.fromInsert insert
          fromODBCException e =
            (err400 MSSQLError "insert query exception") {qeInternal = Just (ExtraInternal $ odbcExceptionToJSONValue e)}

      -- Execute the INSERT query and fetch the primary key values
      primaryKeyValues <- Tx.buildGenericTxE fromODBCException $ \conn -> ODBCI.query conn (ODBC.renderQuery insertQuery)
      let withSelect = generateWithSelect primaryKeyValues
          -- WITH [with_alias] AS (select_query)
          withExpression = With $ pure $ Aliased withSelect withAlias

      mutationOutputSelect <- mkMutationOutputSelect stringifyNum withAlias $ _aiOutput insert
      let (checkCondition, _) = _aiCheckCond $ _aiData insert

      -- The check constraint is translated to boolean expression
      checkBoolExp <-
        V.runValidate (runFromIr $ runReaderT (fromGBoolExp checkCondition) (EntityAlias withAlias))
          `onLeft` (throw500 . tshow)

      -- SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
      let mutationOutputCheckConstraintSelect = selectMutationOutputAndCheckCondition withAlias mutationOutputSelect checkBoolExp

          -- WITH "with_alias" AS (<table_select>)
          -- SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
          finalSelect = mutationOutputCheckConstraintSelect {selectWith = Just withExpression}

      (responseText, checkConditionInt) <- Tx.singleRowQueryE fromMSSQLTxError (toQueryFlat $ TQ.fromSelect finalSelect)
      unless (checkConditionInt == (0 :: Int)) $
        throw400 PermissionError "check constraint of an insert permission has failed"
      pure $ encJFromText responseText

    columnFieldExpression :: ODBCI.Column -> Expression
    columnFieldExpression column =
      ColumnExpression $ TSQL.FieldName (ODBCI.columnName column) withSelectTableAlias

    generateWithSelect :: [[(ODBCI.Column, ODBC.Value)]] -> Select
    generateWithSelect pkeyValues =
      emptySelect
        { selectProjections = [StarProjection],
          selectFrom = Just $ FromQualifiedTable $ Aliased table withSelectTableAlias,
          selectWhere = whereExpression
        }
      where
        -- WHERE (column1 = value1 AND column2 = value2) OR (column1 = value3 AND column2 = value4)
        whereExpression =
          let mkColumnEqExpression (column, value) =
                OpExpression EQ' (columnFieldExpression column) (ValueExpression value)
           in Where $ pure $ OrExpression $ map (AndExpression . map mkColumnEqExpression) pkeyValues

-- Delete

-- | Executes a Delete IR AST and return results as JSON.
executeDelete ::
  MonadError QErr m =>
  UserInfo ->
  Bool ->
  SourceConfig 'MSSQL ->
  AnnDelG 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (ExceptT QErr IO EncJSON)
executeDelete userInfo stringifyNum sourceConfig deleteOperation = do
  preparedDelete <- traverse (prepareValueQuery $ _uiSession userInfo) deleteOperation
  let pool = _mscConnectionPool sourceConfig
  pure $ withMSSQLPool pool $ Tx.runTxE fromMSSQLTxError (buildDeleteTx preparedDelete stringifyNum)

-- | Converts a Delete IR AST to a transaction of three delete sql statements.
--
-- A GraphQL delete mutation does two things:
--
-- 1. Deletes rows in a table according to some predicate
-- 2. (Potentially) returns the deleted rows (including relationships) as JSON
--
-- In order to complete these 2 things we need 3 SQL statements:
--
-- 1. SELECT INTO <temp_table> WHERE <false> - creates a temporary table
--    with the same schema as the original table in which we'll store the deleted rows
--    from the table we are deleting
-- 2. DELETE FROM with OUTPUT - deletes the rows from the table and inserts the
--   deleted rows to the temporary table from (1)
-- 3. SELECT - constructs the @returning@ query from the temporary table, including
--   relationships with other tables.
buildDeleteTx ::
  AnnDel 'MSSQL ->
  Bool ->
  Tx.TxET QErr IO EncJSON
buildDeleteTx deleteOperation stringifyNum = do
  let withAlias = "with_alias"
      createTempTableQuery =
        toQueryFlat $
          TQ.fromSelectIntoTempTable $
            TSQL.toSelectIntoTempTable tempTableNameDeleted (dqp1Table deleteOperation) (dqp1AllCols deleteOperation)
  -- Create a temp table
  Tx.unitQueryE fromMSSQLTxError createTempTableQuery
  let deleteQuery = TQ.fromDelete <$> TSQL.fromDelete deleteOperation
  deleteQueryValidated <- toQueryFlat <$> V.runValidate (runFromIr deleteQuery) `onLeft` (throw500 . tshow)
  -- Execute DELETE statement
  Tx.unitQueryE fromMSSQLTxError deleteQueryValidated
  mutationOutputSelect <- mkMutationOutputSelect stringifyNum withAlias $ dqp1Output deleteOperation
  let withSelect =
        emptySelect
          { selectProjections = [StarProjection],
            selectFrom = Just $ FromTempTable $ Aliased tempTableNameDeleted "deleted_alias"
          }
      finalMutationOutputSelect = mutationOutputSelect {selectWith = Just $ With $ pure $ Aliased withSelect withAlias}
      mutationOutputSelectQuery = toQueryFlat $ TQ.fromSelect finalMutationOutputSelect
  -- Execute SELECT query and fetch mutation response
  encJFromText <$> Tx.singleRowQueryE fromMSSQLTxError mutationOutputSelectQuery

-- | Executes an Update IR AST and return results as JSON.
executeUpdate ::
  MonadError QErr m =>
  UserInfo ->
  Bool ->
  SourceConfig 'MSSQL ->
  AnnotatedUpdateG 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (ExceptT QErr IO EncJSON)
executeUpdate userInfo stringifyNum sourceConfig updateOperation = do
  preparedUpdate <- traverse (prepareValueQuery $ _uiSession userInfo) updateOperation
  let pool = _mscConnectionPool sourceConfig
  if null $ updateOperations . _auBackend $ updateOperation
    then pure $ pure $ IR.buildEmptyMutResp $ _auOutput preparedUpdate
    else pure $ withMSSQLPool pool $ Tx.runTxE fromMSSQLTxError (buildUpdateTx preparedUpdate stringifyNum)

-- | Converts an Update IR AST to a transaction of three update sql statements.
--
-- A GraphQL update mutation does two things:
--
-- 1. Update rows in a table according to some predicate
-- 2. (Potentially) returns the updated rows (including relationships) as JSON
--
-- In order to complete these 2 things we need 3 SQL statements:
--
-- 1. SELECT INTO <temp_table> WHERE <false> - creates a temporary table
--    with the same schema as the original table in which we'll store the updated rows
--    from the table we are deleting
-- 2. UPDATE SET FROM with OUTPUT - updates the rows from the table and inserts the
--   updated rows to the temporary table from (1)
-- 3. SELECT - constructs the @returning@ query from the temporary table, including
--   relationships with other tables.
buildUpdateTx ::
  AnnotatedUpdate 'MSSQL ->
  Bool ->
  Tx.TxET QErr IO EncJSON
buildUpdateTx updateOperation stringifyNum = do
  let withAlias = "with_alias"
      createTempTableQuery =
        toQueryFlat $
          TQ.fromSelectIntoTempTable $
            TSQL.toSelectIntoTempTable tempTableNameUpdated (_auTable updateOperation) (_auAllCols updateOperation)
  -- Create a temp table
  Tx.unitQueryE fromMSSQLTxError createTempTableQuery
  let updateQuery = TQ.fromUpdate <$> TSQL.fromUpdate updateOperation
  updateQueryValidated <- toQueryFlat <$> V.runValidate (runFromIr updateQuery) `onLeft` (throw500 . tshow)
  -- Execute UPDATE statement
  Tx.unitQueryE fromMSSQLTxError updateQueryValidated
  mutationOutputSelect <- mkMutationOutputSelect stringifyNum withAlias $ _auOutput updateOperation
  let checkCondition = _auCheck updateOperation
  -- The check constraint is translated to boolean expression
  checkBoolExp <-
    V.runValidate (runFromIr $ runReaderT (fromGBoolExp checkCondition) (EntityAlias withAlias))
      `onLeft` (throw500 . tshow)

  let withSelect =
        emptySelect
          { selectProjections = [StarProjection],
            selectFrom = Just $ FromTempTable $ Aliased tempTableNameUpdated "updated_alias"
          }
      mutationOutputCheckConstraintSelect = selectMutationOutputAndCheckCondition withAlias mutationOutputSelect checkBoolExp
      finalSelect = mutationOutputCheckConstraintSelect {selectWith = Just $ With $ pure $ Aliased withSelect withAlias}

  -- Execute SELECT query to fetch mutation response and check constraint result
  (responseText, checkConditionInt) <- Tx.singleRowQueryE fromMSSQLTxError (toQueryFlat $ TQ.fromSelect finalSelect)
  -- Drop the temp table
  Tx.unitQueryE fromMSSQLTxError $ toQueryFlat $ dropTempTableQuery tempTableNameUpdated
  -- Raise an exception if the check condition is not met
  unless (checkConditionInt == (0 :: Int)) $
    throw400 PermissionError "check constraint of an update permission has failed"
  pure $ encJFromText responseText

-- | Generate a SQL SELECT statement which outputs the mutation response
--
-- For multi row inserts:
-- SELECT (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows], (select_from_returning) AS [returning] FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--
-- For single row insert: the selection set is translated to SQL query using @'mkSQLSelect'
mkMutationOutputSelect ::
  (MonadError QErr m) =>
  Bool ->
  Text ->
  MutationOutputG 'MSSQL Void Expression ->
  m Select
mkMutationOutputSelect stringifyNum withAlias = \case
  IR.MOutMultirowFields multiRowFields -> do
    projections <- forM multiRowFields $ \(fieldName, field') -> do
      let mkProjection = ExpressionProjection . flip Aliased (getFieldNameTxt fieldName) . SelectExpression
      mkProjection <$> case field' of
        IR.MCount -> pure $ countSelect withAlias
        IR.MExp t -> pure $ textSelect t
        IR.MRet returningFields -> mkSelect stringifyNum withAlias JASMultipleRows returningFields
    let forJson = JsonFor $ ForJson JsonSingleton NoRoot
    pure emptySelect {selectFor = forJson, selectProjections = projections}
  IR.MOutSinglerowObject singleRowField -> mkSelect stringifyNum withAlias JASSingleObject singleRowField

-- | Generate a SQL SELECT statement which outputs the mutation response and check constraint result
--
-- The check constraint boolean expression is evaluated on mutated rows in a CASE expression so that
-- the int value "0" is returned when check constraint is true otherwise the int value "1" is returned.
-- We use "SUM" aggregation on the returned value and if check constraint on any row is not met, the summed
-- value will not equal to "0" (always > 1).
--
--   <check_constraint_select> :=
--     SELECT SUM(CASE WHEN <check_boolean_expression> THEN 0 ELSE 1 END) FROM [with_alias]
--
--   <mutation_output_select> :=
--     SELECT (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows], (select_from_returning) AS [returning] FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--
-- SELECT (<mutation_output_select>) AS [mutation_response], (<check_constraint_select>) AS [check_constraint_select]
selectMutationOutputAndCheckCondition :: Text -> Select -> Expression -> Select
selectMutationOutputAndCheckCondition alias mutationOutputSelect checkBoolExp =
  let mutationOutputProjection =
        ExpressionProjection $ Aliased (SelectExpression mutationOutputSelect) "mutation_response"
      checkConstraintProjection =
        -- apply ISNULL() to avoid check constraint select statement yielding empty rows
        ExpressionProjection $
          Aliased (FunctionExpression "ISNULL" [SelectExpression checkConstraintSelect, ValueExpression (ODBC.IntValue 0)]) "check_constraint_select"
   in emptySelect {selectProjections = [mutationOutputProjection, checkConstraintProjection]}
  where
    checkConstraintSelect =
      let zeroValue = ValueExpression $ ODBC.IntValue 0
          oneValue = ValueExpression $ ODBC.IntValue 1
          caseExpression = ConditionalExpression checkBoolExp zeroValue oneValue
          sumAggregate = OpAggregate "SUM" [caseExpression]
       in emptySelect
            { selectProjections = [AggregateProjection (Aliased sumAggregate "check")],
              selectFrom = Just $ TSQL.FromIdentifier alias
            }

mkSelect ::
  MonadError QErr m =>
  Bool ->
  Text ->
  JsonAggSelect ->
  Fields (AnnFieldG 'MSSQL Void Expression) ->
  m Select
mkSelect stringifyNum withAlias jsonAggSelect annFields = do
  let annSelect = IR.AnnSelectG annFields (IR.FromIdentifier $ FIIdentifier withAlias) IR.noTablePermissions IR.noSelectArgs stringifyNum
  V.runValidate (runFromIr $ mkSQLSelect jsonAggSelect annSelect) `onLeft` (throw500 . tshow)

-- SELECT COUNT(*) AS "count" FROM [with_alias]
countSelect :: Text -> Select
countSelect withAlias =
  let countProjection = AggregateProjection $ Aliased (CountAggregate StarCountable) "count"
   in emptySelect
        { selectProjections = [countProjection],
          selectFrom = Just $ TSQL.FromIdentifier withAlias
        }

-- SELECT '<text-value>' AS "exp"
textSelect :: Text -> Select
textSelect t =
  let textProjection = ExpressionProjection $ Aliased (ValueExpression (ODBC.TextValue t)) "exp"
   in emptySelect {selectProjections = [textProjection]}

-- subscription

msDBSubscriptionPlan ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  Maybe G.Name ->
  RootFieldMap (QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL)) ->
  m (LiveQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL))
msDBSubscriptionPlan UserInfo {_uiSession, _uiRole} _sourceName sourceConfig namespace rootFields = do
  (reselect, prepareState) <- planSubscription (OMap.mapKeys _rfaAlias rootFields) _uiSession
  cohortVariables <- prepareStateCohortVariables sourceConfig _uiSession prepareState
  let parameterizedPlan = ParameterizedLiveQueryPlan _uiRole $ MultiplexedQuery' reselect

  pure $
    LiveQueryPlan parameterizedPlan sourceConfig cohortVariables namespace

prepareStateCohortVariables :: (MonadError QErr m, MonadIO m, MonadBaseControl IO m) => SourceConfig 'MSSQL -> SessionVariables -> PrepareState -> m CohortVariables
prepareStateCohortVariables sourceConfig session prepState = do
  (namedVars, posVars) <- validateVariables sourceConfig session prepState
  let PrepareState {sessionVariables} = prepState
  pure $
    mkCohortVariables
      sessionVariables
      session
      namedVars
      posVars

-- | Ensure that the set of variables (with value instantiations) that occur in
-- a (RQL) query produce a well-formed and executable (SQL) query when
-- considered in isolation.
--
-- This helps avoiding cascading failures in multiplexed queries.
--
-- c.f. https://github.com/hasura/graphql-engine-mono/issues/1210.
validateVariables ::
  (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
  SourceConfig 'MSSQL ->
  SessionVariables ->
  PrepareState ->
  m (ValidatedQueryVariables, ValidatedSyntheticVariables)
validateVariables sourceConfig sessionVariableValues prepState = do
  let PrepareState {sessionVariables, namedArguments, positionalArguments} = prepState

      -- We generate a single 'canary' query in the form:
      --
      -- SELECT ... [session].[x-hasura-foo] as [x-hasura-foo], ... as a, ... as b, ...
      -- FROM OPENJSON('...')
      -- WITH ([x-hasura-foo] NVARCHAR(MAX)) as [session]
      --
      -- where 'a', 'b', etc. are aliases given to positional arguments.
      -- Named arguments and session variables are aliased to themselves.
      --
      -- The idea being that if the canary query succeeds we can be
      -- reasonably confident that adding these variables to a query being
      -- polled will not crash the poller.

      occSessionVars =
        filterSessionVariables
          (\k _ -> Set.member k sessionVariables)
          sessionVariableValues

      expSes, expNamed, expPos :: [Aliased Expression]
      expSes = sessionReference <$> getSessionVariables occSessionVars
      expNamed =
        map
          ( \(n, v) -> Aliased (ValueExpression (RQLColumn.cvValue v)) (G.unName n)
          )
          $ Map.toList $ namedArguments

      -- For positional args we need to be a bit careful not to capture names
      -- from expNamed and expSes (however unlikely)
      expPos =
        zipWith
          (\n v -> Aliased (ValueExpression (RQLColumn.cvValue v)) n)
          (freshVars (expNamed <> expSes))
          positionalArguments

      projAll :: [Projection]
      projAll = map ExpressionProjection (expSes <> expNamed <> expPos)

      canaryQuery =
        if null projAll
          then Nothing
          else
            Just $
              renderQuery
                emptySelect
                  { selectProjections = projAll,
                    selectFrom = sessionOpenJson occSessionVars
                  }

  onJust
    canaryQuery
    ( \q -> do
        _ :: [[ODBC.Value]] <- withMSSQLPool (_mscConnectionPool sourceConfig) (`ODBC.query` q)
        pure ()
    )

  pure
    ( ValidatedVariables $ txtEncodedColVal <$> namedArguments,
      ValidatedVariables $ txtEncodedColVal <$> positionalArguments
    )
  where
    renderQuery :: Select -> ODBC.Query
    renderQuery = toQueryFlat . fromSelect

    freshVars :: [Aliased a] -> [Text]
    freshVars boundNames = filter (not . (`elem` map aliasedAlias boundNames)) chars

    -- Infinite list of expression aliases.
    chars :: [Text]
    chars = [y T.<>> x | y <- [""] <|> chars, x <- ['a' .. 'z']]

    sessionOpenJson :: SessionVariables -> Maybe From
    sessionOpenJson occSessionVars =
      nonEmpty (getSessionVariables occSessionVars)
        <&> \fields ->
          FromOpenJson $
            Aliased
              ( OpenJson
                  (ValueExpression $ ODBC.TextValue $ lbsToTxt $ J.encode occSessionVars)
                  (pure (sessField <$> fields))
              )
              "session"

    sessField :: Text -> JsonFieldSpec
    sessField var = StringField var Nothing

    sessionReference :: Text -> Aliased Expression
    sessionReference var = Aliased (ColumnExpression (TSQL.FieldName var "session")) var

--------------------------------------------------------------------------------
-- Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)
--------------------------------------------------------------------------------

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that SQL Server can query against.
--
-- XXX: Currently unimplemented; the Postgres implementation uses
-- @jsonb_to_recordset@ to query the remote relationship, however this
-- functionality doesn't exist in SQL Server.
--
-- NOTE: The following typeclass constraints will be necessary when implementing
-- this function for real:
--
-- @
--   MonadQueryTags m
--   Backend 'MSSQL
-- @
msDBRemoteRelationshipPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap RQLTypes.FieldName (RQLTypes.Column 'MSSQL, RQLTypes.ScalarType 'MSSQL) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  RQLTypes.FieldName ->
  (RQLTypes.FieldName, SourceRelationshipSelection 'MSSQL Void UnpreparedValue) ->
  m (DBStepInfo 'MSSQL)
msDBRemoteRelationshipPlan _userInfo _sourceName _sourceConfig _lhs _lhsSchema _argumentId _relationship = do
  throw500 "mkDBRemoteRelationshipPlan: SQL Server (MSSQL) does not currently support generalized joins."
