module Hasura.GraphQL.Execute.LiveQuery.Explain
  ( LiveQueryPlanExplanation
  , explainLiveQueryPlan
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.TH                              as J
import qualified Database.PG.Query                          as Q

import           Control.Monad.Trans.Control                (MonadBaseControl)

import           Hasura.Backends.Postgres.Execute.LiveQuery
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.RQL.Types


----------------------------------------------------------------------------------------------------
-- Explain API

data LiveQueryPlanExplanation
  = LiveQueryPlanExplanation
  { _lqpeSql       :: !Text
  , _lqpePlan      :: ![Text]
  , _lqpeVariables :: !CohortVariables
  } deriving (Show)
$(J.deriveToJSON hasuraJSON ''LiveQueryPlanExplanation)

explainLiveQueryPlan
  :: ( MonadError QErr m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => LiveQueryPlan 'Postgres MultiplexedQuery -> m LiveQueryPlanExplanation
explainLiveQueryPlan plan = do
  let parameterizedPlan = _lqpParameterizedPlan plan
      pgExecCtx = _pscExecCtx $ _lqpSourceConfig plan
      queryText = Q.getQueryText . unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <- liftEitherM $ runExceptT $ runLazyTx pgExecCtx Q.ReadOnly $
                      map runIdentity <$> executeQuery explainQuery [(cohortId, _lqpVariables plan)]
  pure $ LiveQueryPlanExplanation queryText explanationLines $ _lqpVariables plan
