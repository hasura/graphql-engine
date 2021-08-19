-- | Planning T-SQL queries and subscriptions.

module Hasura.Backends.BigQuery.Plan
  ( planNoPlan
  ) where

import           Hasura.Prelude

import qualified Data.Text.Lazy                  as LT

import           Control.Monad.Validate
import           Data.Aeson.Text
import           Data.Text.Extended

import qualified Hasura.Base.Error               as E
import qualified Hasura.GraphQL.Parser           as GraphQL
import qualified Hasura.RQL.Types.Column         as RQL

import           Hasura.Backends.BigQuery.FromIr as BigQuery
import           Hasura.Backends.BigQuery.Types
import           Hasura.RQL.IR
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Session


--------------------------------------------------------------------------------
-- Top-level planner

planNoPlan ::
     MonadError E.QErr m
  => FromIrConfig
  -> UserInfo
  -> QueryDB 'BigQuery (Const Void) (GraphQL.UnpreparedValue 'BigQuery)
  -> m Select
planNoPlan fromIrConfig userInfo queryDB = do
  rootField <- traverse (prepareValueNoPlan (_uiSession userInfo)) queryDB
  runValidate (BigQuery.runFromIr fromIrConfig (BigQuery.fromRootField rootField))
    `onLeft` (E.throw400 E.NotSupported . (tshow :: NonEmpty Error -> Text))


--------------------------------------------------------------------------------
-- Resolving values

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan ::
     (MonadError E.QErr m)
  => SessionVariables
  -> GraphQL.UnpreparedValue 'BigQuery
  -> m Expression
prepareValueNoPlan sessionVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> pure globalSessionExpression
    -- To be honest, I'm not sure if it's indeed the JSON_VALUE operator we need here...
    GraphQL.UVSessionVar typ text ->
      case typ of
        CollectableTypeScalar scalarType ->
          pure
            (CastExpression
               (JsonValueExpression
                  globalSessionExpression
                  (FieldPath RootPath (toTxt text)))
               scalarType)
        CollectableTypeArray {} ->
          throwError $ E.internalError "Cannot currently prepare array types in BigQuery."
    GraphQL.UVParameter _ RQL.ColumnValue {..} -> pure (ValueExpression cvValue)
  where
    globalSessionExpression =
      ValueExpression
        (StringValue (LT.toStrict (encodeToLazyText sessionVariables)))
