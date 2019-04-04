module Hasura.GraphQL.Execute.Plan
  ( ReusablePlan(..)
  , PlanCache
  , getPlan
  , addPlan
  , initPlanCache
  , clearPlanCache
  , dumpPlanCache
  ) where

import qualified Hasura.Cache                           as Cache
import           Hasura.Prelude

import qualified Data.Aeson                             as J

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
  { _unPlanCache :: Cache.UnboundedCache PlanId ReusablePlan
  }

data ReusablePlan
  = RPQuery !EQ.ReusableQueryPlan
  | RPSubs !LQ.SubsPlan

instance J.ToJSON ReusablePlan where
  toJSON = \case
    RPQuery queryPlan -> J.toJSON queryPlan
    RPSubs subsPlan -> J.toJSON subsPlan

initPlanCache :: IO PlanCache
initPlanCache = PlanCache <$> Cache.initCache

getPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> PlanCache -> IO (Maybe ReusablePlan)
getPlan schemaVer rn opNameM q (PlanCache planCache) =
  Cache.lookup planCache planId
  where
    planId = PlanId schemaVer rn opNameM q

addPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> ReusablePlan -> PlanCache -> IO ()
addPlan schemaVer rn opNameM q queryPlan (PlanCache planCache) =
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
