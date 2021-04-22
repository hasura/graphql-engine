-- | Planning T-SQL queries and subscriptions.

module Hasura.Backends.BigQuery.Plan
  ( planNoPlan
  , planToForest
  ) where

import           Data.Aeson.Text
import qualified Data.Text.Lazy as LT
import           Data.Tree
import qualified Hasura.Backends.BigQuery.DataLoader.Plan as DataLoader
import           Hasura.Prelude hiding (first)
import           Hasura.SQL.Types
import           Hasura.Session


import           Control.Monad.Validate
import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser as GraphQL
import qualified Hasura.RQL.Types.Column as RQL

import           Hasura.Backends.BigQuery.FromIr as BigQuery
import           Hasura.Backends.BigQuery.Types as BigQuery
import           Hasura.GraphQL.Context
import qualified Hasura.RQL.Types.Error as RQL
import           Hasura.SQL.Backend

-- --------------------------------------------------------------------------------
-- -- Top-level planner

planToForest ::
     MonadError RQL.QErr m
  => UserInfo
  -> QueryDB 'BigQuery (GraphQL.UnpreparedValue 'BigQuery)
  -> m (Forest DataLoader.PlannedAction)
planToForest userInfo qrf = do
  select <- planNoPlan userInfo qrf
  let (!_headAndTail, !plannedActionsList) =
        DataLoader.runPlan
          (DataLoader.planSelectHeadAndTail Nothing Nothing select)
      !actionsForest = DataLoader.actionsForest id plannedActionsList
  pure actionsForest

planNoPlan ::
     MonadError RQL.QErr m
  => UserInfo
  -> QueryDB 'BigQuery (GraphQL.UnpreparedValue 'BigQuery)
  -> m Select
planNoPlan userInfo queryDB = do
  rootField <- traverseQueryDB (prepareValueNoPlan (_uiSession userInfo)) queryDB
  select <-
    runValidate (BigQuery.runFromIr (BigQuery.fromRootField rootField))
    `onLeft` (RQL.throw400 RQL.NotSupported . (tshow :: NonEmpty Error -> Text))
  pure
    select
      { selectFor =
          case selectFor select of
            NoFor           -> NoFor
            JsonFor forJson -> JsonFor forJson {jsonRoot = Root "root"}
      }

--------------------------------------------------------------------------------
-- Resolving values

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan ::
     (MonadError RQL.QErr m)
  => SessionVariables
  -> GraphQL.UnpreparedValue 'BigQuery
  -> m BigQuery.Expression
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
          throwError $ RQL.internalError "Cannot currently prepare array types in BigQuery."
    GraphQL.UVParameter _ RQL.ColumnValue {..} -> pure (ValueExpression cvValue)
  where
    globalSessionExpression =
      ValueExpression
        (StringValue (LT.toStrict (encodeToLazyText sessionVariables)))
