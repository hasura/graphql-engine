module Hasura.GraphQL.Execute.Plan
  ( ReusablePlan(..)
  , PlanCache
  , getPlans
  , addPlans
  , initPlanCache
  , clearPlanCache
  , dumpPlanCache
  ) where

import qualified Hasura.Cache                           as Cache
import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Sequence                          as Seq

import qualified Hasura.GraphQL.Execute.LiveQuery       as LQ
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import           Hasura.RQL.Types

data PlanId
  = PlanId
  { _piSchemaCacheVersion :: !SchemaCacheVer
  , _piRole               :: !RoleName
  , _piOperationName      :: !(Maybe GH.OperationName)
  , _piQuery              :: !GH.GQLQueryText
  } deriving (Show, Eq, Generic)

instance Hashable PlanId

instance J.ToJSON PlanId where
  toJSON (PlanId scVer rn opNameM query) =
    J.object
    [ "schema_cache_version" J..= scVer
    , "role" J..= rn
    , "operation" J..= opNameM
    , "query" J..= query
    ]

newtype PlanCache
  = PlanCache
  { _unPlanCache :: Cache.UnboundedCache PlanId (Seq.Seq ReusablePlan)
  -- ^ We cache resusable plan for each top-level field, using a hash key
  -- that represents the entire query. This is pretty weird; I think we'd
  -- prefer if the cache were more granular as long as we're already using
  -- separate plans for top-level fields.
  --
  -- We would need to at least parse the query again to do that. Without
  -- benchmarks I can't tell how much more work is acceptable to do before we
  -- lose the benefits of having the cache.
  --
  -- The cache payload should be non-empty.
  }

data ReusablePlan
  = RPQuery !EQ.ReusableQueryPlan
  | RPSubs !LQ.ReusableLiveQueryPlan

instance J.ToJSON ReusablePlan where
  toJSON = \case
    RPQuery queryPlan -> J.toJSON queryPlan
    RPSubs subsPlan -> J.toJSON subsPlan

initPlanCache :: IO PlanCache
initPlanCache = PlanCache <$> Cache.initCache

getPlans
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> PlanCache -> IO (Maybe (Seq.Seq ReusablePlan))
getPlans schemaVer rn opNameM q (PlanCache planCache) =
  Cache.lookup planCache planId
  where
    planId = PlanId schemaVer rn opNameM q

addPlans
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> Seq.Seq ReusablePlan -> PlanCache -> IO ()
addPlans schemaVer rn opNameM q queryPlan (PlanCache planCache) =
  Cache.insert planCache planId queryPlan
  where
    planId = PlanId schemaVer rn opNameM q

clearPlanCache :: PlanCache -> IO ()
clearPlanCache (PlanCache planCache) =
  Cache.clearCache planCache

dumpPlanCache :: PlanCache -> IO J.Value
dumpPlanCache (PlanCache cache) =
  J.toJSON <$> Cache.mapCache dumpEntry cache
  where
    dumpEntry (planId, plan) =
      J.object
      [ "id" J..= planId
      , "plan" J..= plan
      ]
