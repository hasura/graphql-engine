{-# LANGUAGE MonadComprehensions #-}

-- | Planning T-SQL queries and subscriptions.
module Hasura.Backends.BigQuery.Plan
  ( planNoPlan,
  )
where

import Control.Monad.Validate
import Data.Aeson.Text
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text.Extended
import Data.Text.Lazy qualified as LT
import Hasura.Backends.BigQuery.DDL (scalarTypeFromColumnType)
import Hasura.Backends.BigQuery.FromIr as BigQuery
import Hasura.Backends.BigQuery.Types
import Hasura.Base.Error qualified as E
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.SQL.Types
import Hasura.Session

--------------------------------------------------------------------------------
-- Top-level planner

planNoPlan ::
  (MonadError E.QErr m) =>
  FromIrConfig ->
  UserInfo ->
  QueryDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  m Select
planNoPlan fromIrConfig userInfo queryDB = do
  rootField <- traverse (prepareValueNoPlan (_uiSession userInfo)) queryDB

  (select, FromIrWriter {fromIrWriterNativeQueries}) <-
    runValidate (BigQuery.runFromIr fromIrConfig (BigQuery.fromRootField rootField))
      `onLeft` (E.throw400 E.NotSupported . (tshow :: NonEmpty Error -> Text))

  -- Native queries used within this query need to be converted into CTEs.
  -- These need to come before any other CTEs in case those CTEs also depend on
  -- the native queries.
  let nativeQueries :: Maybe With
      nativeQueries = do
        ctes <- NE.nonEmpty (Map.toList fromIrWriterNativeQueries)
        pure (With [Aliased query aliasedAlias | (Aliased {aliasedAlias}, query) <- ctes])

  pure select {selectWith = nativeQueries <> selectWith select}

--------------------------------------------------------------------------------
-- Resolving values

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan ::
  (MonadError E.QErr m) =>
  SessionVariables ->
  UnpreparedValue 'BigQuery ->
  m Expression
prepareValueNoPlan sessionVariables =
  \case
    UVLiteral x -> pure x
    UVSession -> pure globalSessionExpression
    -- To be honest, I'm not sure if it's indeed the JSON_VALUE operator we need here...
    UVSessionVar typ text ->
      case typ of
        CollectableTypeScalar scalarType ->
          pure
            ( CastExpression
                ( JsonValueExpression
                    globalSessionExpression
                    (FieldPath RootPath (toTxt text))
                )
                scalarType
            )
        CollectableTypeArray {} ->
          throwError $ E.internalError "Cannot currently prepare array types in BigQuery."
    UVParameter _ RQL.ColumnValue {..} ->
      pure (ValueExpression (TypedValue (scalarTypeFromColumnType cvType) cvValue))
  where
    globalSessionExpression =
      ValueExpression
        ( TypedValue
            StringScalarType
            (StringValue (LT.toStrict (encodeToLazyText sessionVariables)))
        )
