{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Defines common functionality for building MSSQL execution plans for IR ASTs.
module Hasura.Backends.MSSQL.Execute.MutationResponse
  ( mkMutationOutputSelect,
    selectMutationOutputAndCheckCondition,
  )
where

import Control.Monad.Validate qualified as V
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr as TSQL
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types

-- ** Mutation response

-- | Generate a SQL SELECT statement which outputs the mutation response
--
-- For multi row inserts:
-- SELECT (SELECT COUNT(*) FROM [with_alias]) AS [affected_rows], (select_from_returning) AS [returning] FOR JSON PATH, INCLUDE_NULL_VALUES, WITHOUT_ARRAY_WRAPPER
--
-- For single row insert: the selection set is translated to SQL query using @'mkSQLSelect'
mkMutationOutputSelect ::
  (MonadError QErr m) =>
  StringifyNumbers ->
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
          Aliased (FunctionApplicationExpression $ FunExpISNULL (SelectExpression checkConstraintSelect) (ValueExpression (ODBC.IntValue 0))) "check_constraint_select"
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
  StringifyNumbers ->
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
