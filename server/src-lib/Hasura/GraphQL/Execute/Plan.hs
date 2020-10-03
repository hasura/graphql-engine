module Hasura.GraphQL.Execute.Plan
  ( ReusablePlan(..)
  , PlanCache
  , PlanCacheOptions(..)
  , mkPlanCacheOptions
  , getPlan
  , addPlan
  , initPlanCache
  , clearPlanCache
  , dumpPlanCache
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Session

import qualified Hasura.Cache.Bounded                   as Cache
-- import qualified Hasura.GraphQL.Execute.LiveQuery       as LQ
-- import qualified Hasura.GraphQL.Execute.Query           as EQ

import qualified Hasura.GraphQL.Execute.Types           as ET
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

{- Note [Temporarily disabling query plan caching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Caching the incoming queries for re-usability is *temporarily* disabled.
This is being done as part of rewriting GraphQL schema generation and
execution (See https://github.com/hasura/graphql-engine/pull/4111)
until we figure out if we need query plan caching.

The code related to query caching in GraphQL query execution code path
is just commented with referring to this note. The relavent variables are
commented inline (Ex: {- planCache -}) to help authors in re-enabling
the query caching feature (if needed).
-}

data PlanId
  = PlanId
  { _piSchemaCacheVersion :: !SchemaCacheVer
  , _piRole               :: !RoleName
  , _piOperationName      :: !(Maybe GH.OperationName)
  , _piQuery              :: !GH.GQLQueryText
  , _piQueryType          :: !ET.GraphQLQueryType
  } deriving (Show, Eq, Ord, Generic)

instance Hashable PlanId

instance J.ToJSON PlanId where
  toJSON (PlanId scVer rn opNameM query queryType) =
    J.object
    [ "schema_cache_version" J..= scVer
    , "role" J..= rn
    , "operation" J..= opNameM
    , "query" J..= query
    , "query_type" J..= queryType
    ]

newtype PlanCache
  = PlanCache {_unPlanCache :: Cache.BoundedCache PlanId ReusablePlan}

data ReusablePlan = ReusablePlan

-- See Note [Temporarily disabling query plan caching]
-- data ReusablePlan
--   = RPQuery !EQ.ReusableQueryPlan
--   | RPSubs !LQ.ReusableLiveQueryPlan

-- instance J.ToJSON ReusablePlan where
--   toJSON = \case
--     RPQuery queryPlan  -> J.toJSON queryPlan
--     RPSubs subsPlan    -> J.toJSON subsPlan

newtype PlanCacheOptions
  = PlanCacheOptions { unPlanCacheSize :: Cache.CacheSize }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''PlanCacheOptions)

mkPlanCacheOptions :: Cache.CacheSize -> PlanCacheOptions
mkPlanCacheOptions = PlanCacheOptions

initPlanCache :: PlanCacheOptions -> IO PlanCache
initPlanCache options =
  PlanCache <$> Cache.initialise (unPlanCacheSize options)

getPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> ET.GraphQLQueryType -> PlanCache -> IO (Maybe ReusablePlan)
getPlan schemaVer rn opNameM q queryType (PlanCache planCache) =
  Cache.lookup planId planCache
  where
    planId = PlanId schemaVer rn opNameM q queryType

addPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> ReusablePlan -> ET.GraphQLQueryType -> PlanCache -> IO ()
addPlan schemaVer rn opNameM q queryPlan queryType (PlanCache planCache) =
  Cache.insert planId queryPlan planCache
  where
    planId = PlanId schemaVer rn opNameM q queryType

-- See Note [Temporarily disabling query plan caching]
-- clearPlanCache :: PlanCache -> IO ()
clearPlanCache :: IO ()
clearPlanCache {- (PlanCache planCache) -} =
  pure ()
  -- Cache.clear planCache

-- See Note [Temporarily disabling query plan caching]
-- dumpPlanCache :: PlanCache -> IO J.Value
dumpPlanCache :: IO J.Value
dumpPlanCache {- (PlanCache cache) -} =
  pure $ J.String "Plan cache is temporarily disabled"
  -- J.toJSON . map (map dumpEntry) <$> Cache.getEntries cache
  -- where
  --   dumpEntry (planId, plan) =
  --     J.object
  --     [ "id" J..= planId
  --     , "plan" J..= plan
  --     ]
