-- | Planning T-SQL queries and subscriptions.
module Hasura.Backends.BigQuery.Plan
  ( planNoPlan,
  )
where

import Control.Monad.Validate
import Data.Aeson.Text
import Data.Text.Extended
import Data.Text.Lazy qualified as LT
import Hasura.Backends.BigQuery.FromIr as BigQuery
import Hasura.Backends.BigQuery.Types
import Hasura.Base.Error qualified as E
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.SQL.Backend
import Hasura.SQL.Types
import Hasura.Session

--------------------------------------------------------------------------------
-- Top-level planner

planNoPlan ::
  MonadError E.QErr m =>
  FromIrConfig ->
  UserInfo ->
  QueryDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  m Select
planNoPlan fromIrConfig userInfo queryDB = do
  rootField <- traverse (prepareValueNoPlan (_uiSession userInfo)) queryDB
  runValidate (BigQuery.runFromIr fromIrConfig (BigQuery.fromRootField rootField))
    `onLeft` (E.throw400 E.NotSupported . (tshow :: NonEmpty Error -> Text))

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
    UVParameter _ RQL.ColumnValue {..} -> pure (ValueExpression cvValue)
  where
    globalSessionExpression =
      ValueExpression
        (StringValue (LT.toStrict (encodeToLazyText sessionVariables)))
