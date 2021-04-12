{-# LANGUAGE CPP #-}

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
module Hasura.GraphQL.Execute.LiveQuery
  ( LiveQueryPlan
  , ReusableLiveQueryPlan
  , reuseLiveQueryPlan
  , buildLiveQueryPlan

  , LiveQueryPlanExplanation
  , explainLiveQueryPlan

  , LiveQueriesState(..)
  , initLiveQueriesState
  , dumpLiveQueriesState

  , LiveQueriesOptions(..)
  , BatchSize(..)
  , RefetchInterval(..)
  , mkLiveQueriesOptions

  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery
  ) where

import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.LiveQuery.State

#ifdef __HADDOCK_VERSION__
import           Hasura.Prelude

import           Hasura.GraphQL.Execute.LiveQuery.Poll
#endif
