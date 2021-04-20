{-# LANGUAGE UndecidableInstances #-}

{-|
= Reasonably efficient PostgreSQL live queries

The module implements /query multiplexing/, which is our implementation strategy for live queries
(i.e. GraphQL subscriptions) made against Postgres. Fundamentally, our implementation is built
around polling, which is never ideal, but it’s a lot easier to implement than trying to do something
event-based. To minimize the resource cost of polling, we use /multiplexing/, which is essentially
a two-tier batching strategy.

== The high-level idea

The objective is to minimize the number of concurrent polling workers to reduce database load as
much as possible. A very naïve strategy would be to group identical queries together so we only have
one poller per /unique/ active subscription. That’s a good start, but of course, in practice, most
queries differ slightly. However, it happens that they very frequently /only differ in their
variables/ (that is, GraphQL query variables and session variables), and in those cases, we try to
generated parameterized SQL. This means that the same prepared SQL query can be reused, just with a
different set of variables.

To give a concrete example, consider the following query:

> subscription vote_count($post_id: Int!) {
>   vote_count(where: {post_id: {_eq: $post_id}}) {
>     votes
>   }
> }

No matter what the client provides for @$post_id@, we will always generate the same SQL:

> SELECT votes FROM vote_count WHERE post_id = $1

If multiple clients subscribe to @vote_count@, we can certainly reuse the same prepared query. For
example, imagine we had 10 concurrent subscribers, each listening on a distinct @$post_id@:

> let postIds = [3, 11, 32, 56, 13, 97, 24, 43, 109, 48]

We could iterate over @postIds@ in Haskell, executing the same prepared query 10 times:

> for postIds $ \postId ->
>   Q.listQE defaultTxErrorHandler preparedQuery (Identity postId) True

Sadly, that on its own isn’t good enough. The overhead of running each query is large enough that
Postgres becomes overwhelmed if we have to serve lots of concurrent subscribers. Therefore, what we
want to be able to do is somehow make one query instead of ten.

=== Multiplexing

This is where multiplexing comes in. By taking advantage of Postgres
<https://www.postgresql.org/docs/11/queries-table-expressions.html#QUERIES-LATERAL lateral joins>,
we can do the iteration in Postgres rather than in Haskell, allowing us to pay the query overhead
just once for all ten subscribers. Essentially, lateral joins add 'map'-like functionality to SQL,
so we can run our query once per @$post_id@:

> SELECT results.votes
> FROM unnest($1::integer[]) query_variables (post_id)
> LEFT JOIN LATERAL (
>   SELECT coalesce(json_agg(votes), '[]')
>   FROM vote_count WHERE vote_count.post_id = query_variables.post_id
> ) results ON true

If we generalize this approach just a little bit more, we can apply this transformation to arbitrary
queries parameterized over arbitrary session and query variables!

== Implementation overview

To support query multiplexing, we maintain a tree of the following types, where @>@ should be read
as “contains”:

@
'LiveQueriesState' > 'Poller' > 'Cohort' > 'Subscriber'
@

Here’s a brief summary of each type’s role:

  * A 'Subscriber' is an actual client with an open websocket connection.

  * A 'Cohort' is a set of 'Subscriber's that are all subscribed to the same query /with the exact
    same variables/. (By batching these together, we can do better than multiplexing, since we can
    just query the data once.)

  * A 'Poller' is a worker thread for a single, multiplexed query. It fetches data for a set of
    'Cohort's that all use the same parameterized query, but have different sets of variables.

  * Finally, the 'LiveQueriesState' is the top-level container that holds all the active 'Poller's.

Additional details are provided by the documentation for individual bindings.
-}
module Hasura.GraphQL.Execute.LiveQuery.Plan
  ( CohortId
  , newCohortId
  , CohortIdArray(..)
  , CohortVariablesArray(..)
  , CohortVariables
  , mkCohortVariables
  , ValidatedVariables(..)
  , ValidatedQueryVariables
  , ValidatedSyntheticVariables
  , LiveQueryPlan(..)
  , LiveQueryPlanExplanation(..)
  , ParameterizedLiveQueryPlan(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Extended                as J
import qualified Data.Aeson.TH                      as J
import qualified Data.HashMap.Strict                as Map
import qualified Data.HashSet                       as Set
import qualified Data.UUID.V4                       as UUID
import qualified Database.PG.Query                  as Q
import qualified Database.PG.Query.PTI              as PTI
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified PostgreSQL.Binary.Encoding         as PE

import           Data.UUID                          (UUID)

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.RQL.Types
import           Hasura.Session


----------------------------------------------------------------------------------------------------
-- Cohort

newtype CohortId = CohortId { unCohortId :: UUID }
  deriving (Show, Eq, Hashable, J.ToJSON, J.FromJSON, Q.FromCol)

newCohortId :: (MonadIO m) => m CohortId
newCohortId = CohortId <$> liftIO UUID.nextRandom

data CohortVariables
  = CohortVariables
  { _cvSessionVariables   :: !SessionVariables
-- ^ A set of session variables, pruned to the minimal set actually used by
-- this query. To illustrate the need for this pruning, suppose we have the
-- following query:
--
-- > query {
-- >   articles {
-- >     id
-- >     title
-- >   }
-- > }
--
-- If the select permission on @articles@ is just @{"is_public": true}@, we
-- just generate the SQL query
--
-- > SELECT id, title FROM articles WHERE is_public = true
--
-- which doesn’t use any session variables at all. Therefore, we ought to be
-- able to multiplex all queries of this shape into a single cohort, for quite
-- good performance! But if we don’t prune the session variables, we’ll
-- needlessly split subscribers into several cohorts simply because they have
-- different values for, say, @X-Hasura-User-Id@.
--
-- The 'mkCohortVariables' smart constructor handles pruning the session
-- variables to a minimal set, avoiding this pessimization.
  , _cvQueryVariables     :: !ValidatedQueryVariables
  , _cvSyntheticVariables :: !ValidatedSyntheticVariables
  -- ^ To allow more queries to be multiplexed together, we introduce “synthetic”
  -- variables for /all/ SQL literals in a query, even if they don’t correspond to
  -- any GraphQL variable. For example, the query
  --
  -- > subscription latest_tracks($condition: tracks_bool_exp!) {
  -- >   tracks(where: $tracks_bool_exp) {
  -- >     id
  -- >     title
  -- >   }
  -- > }
  --
  -- might be executed with similar values for @$condition@, such as @{"album_id":
  -- {"_eq": "1"}}@ and @{"album_id": {"_eq": "2"}}@.
  --
  -- Normally, we wouldn’t bother parameterizing over the @1@ and @2@ literals in the
  -- resulting query because we can’t cache that query plan (since different
  -- @$condition@ values could lead to different SQL). However, for live queries, we
  -- can still take advantage of the similarity between the two queries by
  -- multiplexing them together, so we replace them with references to synthetic
  -- variables.
  } deriving (Show, Eq, Generic)
instance Hashable CohortVariables

-- | Builds a cohort's variables by only using the session variables that
-- are required for the subscription
mkCohortVariables
  :: Set.HashSet SessionVariable
  -> SessionVariables
  -> ValidatedQueryVariables
  -> ValidatedSyntheticVariables
  -> CohortVariables
mkCohortVariables requiredSessionVariables sessionVariableValues =
  CohortVariables $ filterSessionVariables (\k _ -> Set.member k requiredSessionVariables)
  sessionVariableValues

instance J.ToJSON CohortVariables where
  toJSON (CohortVariables sessionVars queryVars syntheticVars) =
    J.object [ "session" J..= sessionVars
             , "query" J..= queryVars
             , "synthetic" J..= syntheticVars
             ]

-- These types exist only to use the Postgres array encoding.
newtype CohortIdArray = CohortIdArray { unCohortIdArray :: [CohortId] }
  deriving (Show, Eq)

instance Q.ToPrepArg CohortIdArray where
  toPrepVal (CohortIdArray l) = Q.toPrepValHelper PTI.unknown encoder $ map unCohortId l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

newtype CohortVariablesArray
  = CohortVariablesArray { unCohortVariablesArray :: [CohortVariables] }
  deriving (Show, Eq)

instance Q.ToPrepArg CohortVariablesArray where
  toPrepVal (CohortVariablesArray l) =
    Q.toPrepValHelper PTI.unknown encoder (map J.toJSON l)
    where
      encoder = PE.array 114 . PE.dimensionArray foldl' (PE.encodingArray . PE.json_ast)


----------------------------------------------------------------------------------------------------
-- Variable validation

-- | When running multiplexed queries, we have to be especially careful about user
-- input, since invalid values will cause the query to fail, causing collateral
-- damage for anyone else multiplexed into the same query.  Therefore, we
-- pre-validate variables against Postgres by executing a no-op query of the shape
--
-- > SELECT 'v1'::t1, 'v2'::t2, ..., 'vn'::tn
--
-- so if any variable values are invalid, the error will be caught early.

newtype ValidatedVariables f = ValidatedVariables (f TxtEncodedVal)

deriving instance (Show (f TxtEncodedVal)) => Show (ValidatedVariables f)
deriving instance (Eq (f TxtEncodedVal)) => Eq (ValidatedVariables f)
deriving instance (Hashable (f TxtEncodedVal)) => Hashable (ValidatedVariables f)
deriving instance (J.ToJSON (f TxtEncodedVal)) => J.ToJSON (ValidatedVariables f)
deriving instance (Semigroup (f TxtEncodedVal)) => Semigroup (ValidatedVariables f)
deriving instance (Monoid (f TxtEncodedVal)) => Monoid (ValidatedVariables f)

type ValidatedQueryVariables     = ValidatedVariables (Map.HashMap G.Name)
type ValidatedSyntheticVariables = ValidatedVariables []


----------------------------------------------------------------------------------------------------
-- Live query plans

-- | A self-contained, ready-to-execute live query plan. Contains enough information
-- to find an existing poller that this can be added to /or/ to create a new poller
-- if necessary.

data LiveQueryPlan (b :: BackendType) q
  = LiveQueryPlan
  { _lqpParameterizedPlan :: !(ParameterizedLiveQueryPlan b q)
  , _lqpSourceConfig      :: !(SourceConfig b)
  , _lqpVariables         :: !CohortVariables
  }

data ParameterizedLiveQueryPlan (b :: BackendType) q
  = ParameterizedLiveQueryPlan
  { _plqpRole  :: !RoleName
  , _plqpQuery :: !q
  } deriving (Show)
$(J.deriveToJSON hasuraJSON ''ParameterizedLiveQueryPlan)

data LiveQueryPlanExplanation
  = LiveQueryPlanExplanation
  { _lqpeSql       :: !Text
  , _lqpePlan      :: ![Text]
  , _lqpeVariables :: !CohortVariables
  } deriving (Show)
$(J.deriveToJSON hasuraJSON ''LiveQueryPlanExplanation)
