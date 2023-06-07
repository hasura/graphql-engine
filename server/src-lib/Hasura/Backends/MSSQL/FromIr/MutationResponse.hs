-- | This module defines translation functions that yield the results of
-- mutation requests that return the data of rows that were affected.
module Hasura.Backends.MSSQL.FromIr.MutationResponse
  ( mkMutationOutputSelect,
    selectMutationOutputAndCheckCondition,
  )
where

import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr (FromIr)
import Hasura.Backends.MSSQL.FromIr.Query (fromSelect)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.Returning (MutationOutputG)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common qualified as IR
import Hasura.RQL.Types.Schema.Options qualified as Options

-- | Generate a SQL SELECT statement which outputs the mutation response
--
-- For multi row inserts:
--
--   SELECT
--     (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows],
--     (select_from_returning) AS [returning]
--   FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--
-- For single row insert: the selection set is translated to SQL query using @'fromSelect'
mkMutationOutputSelect ::
  Options.StringifyNumbers ->
  Text ->
  MutationOutputG 'MSSQL Void Expression ->
  FromIr Select
mkMutationOutputSelect stringifyNum withAlias = \case
  IR.MOutMultirowFields multiRowFields -> do
    projections <- forM multiRowFields $ \(fieldName, field') -> do
      let mkProjection = ExpressionProjection . flip Aliased (IR.getFieldNameTxt fieldName) . SelectExpression
      mkProjection <$> case field' of
        IR.MCount -> pure $ countSelect
        IR.MExp t -> pure $ textSelect t
        IR.MRet returningFields -> mkSelect IR.JASMultipleRows returningFields
    let forJson = JsonFor $ ForJson JsonSingleton NoRoot
    pure emptySelect {selectFor = forJson, selectProjections = projections}
  IR.MOutSinglerowObject singleRowField -> mkSelect IR.JASSingleObject singleRowField
  where
    mkSelect ::
      IR.JsonAggSelect ->
      IR.Fields (IR.AnnFieldG 'MSSQL Void Expression) ->
      FromIr Select
    mkSelect jsonAggSelect annFields = do
      let annSelect = IR.AnnSelectG annFields (IR.FromIdentifier $ IR.FIIdentifier withAlias) IR.noTablePermissions IR.noSelectArgs stringifyNum Nothing
      fromSelect jsonAggSelect annSelect

    -- SELECT COUNT(*) AS "count" FROM [with_alias]
    countSelect :: Select
    countSelect =
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

-- | Generate a SQL SELECT statement which outputs both mutation response and
-- check constraint result.
--
-- A @check constraint@ applies to the data that has been changed, while
-- @permissions@ filter the data that is made available.
--
-- This function applies to @insert@ and @update@ mutations.
--
-- The check constraint boolean expression is evaluated on mutated rows in a
-- CASE expression so that the int value "0" is returned when check constraint
-- is true otherwise the int value "1" is returned.  We use "SUM" aggregation on
-- the returned value and if check constraint on any row is not met, the summed
-- value will not equal to "0" (always > 1).
--
--  >   <check_constraint_select> :=
--  >     SELECT SUM([check_sub_query].[check_evaluation])
--  >     FROM
--  >       ( SELECT
--  >           (CASE WHEN <check_boolean_expression> THEN 0 ELSE 1 END) AS [check_evaluation]
--  >         FROM
--  >           [with_alias]
--  >       ) AS [check_sub_query]
--  >
--  >   <mutation_output_select> :=
--  >     SELECT
--  >       (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows],
--  >       (select_from_returning) AS [returning]
--  >     FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--  >
--  >   SELECT
--  >     (<mutation_output_select>) AS [mutation_response],
--  >     (<check_constraint_select>) AS [check_constraint_select]
selectMutationOutputAndCheckCondition :: Text -> Select -> Expression -> Select
selectMutationOutputAndCheckCondition alias mutationOutputSelect checkBoolExp =
  let mutationOutputProjection =
        ExpressionProjection $ Aliased (SelectExpression mutationOutputSelect) "mutation_response"
      checkConstraintProjection =
        -- apply ISNULL() to avoid check constraint select statement yielding empty rows
        ExpressionProjection
          $ Aliased (FunctionApplicationExpression $ FunExpISNULL (SelectExpression checkConstraintSelect) (ValueExpression (ODBC.IntValue 0))) "check_constraint_select"
   in emptySelect {selectProjections = [mutationOutputProjection, checkConstraintProjection]}
  where
    checkConstraintSelect =
      let subQueryAlias = "check_sub_query"
          checkEvaluationFieldName = "check_evaluation"
          sumAggregate =
            OpAggregate
              "SUM"
              [ ColumnExpression
                  $ FieldName
                    { fieldNameEntity = subQueryAlias,
                      fieldName = checkEvaluationFieldName
                    }
              ]
          checkSubQuery =
            let zeroValue = ValueExpression $ ODBC.IntValue 0
                oneValue = ValueExpression $ ODBC.IntValue 1
                caseExpression = ConditionalExpression checkBoolExp zeroValue oneValue
             in emptySelect
                  { selectProjections = [ExpressionProjection (Aliased caseExpression checkEvaluationFieldName)],
                    selectFrom = Just $ TSQL.FromIdentifier alias
                  }
       in emptySelect
            { selectProjections = [AggregateProjection (Aliased sumAggregate "check")],
              selectFrom = Just $ TSQL.FromSelect (Aliased checkSubQuery subQueryAlias)
            }
