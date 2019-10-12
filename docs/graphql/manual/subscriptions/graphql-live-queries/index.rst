Implementing GraphQL live-queries
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Implementing live-queries is painful. Once you have a database query that has all the authorization rules, it might be
possible to incrementally compute the result of the query as events happen. But this is practically challenging to do at
the web-service layer. For databases like Postgres, it is equivalent to the hard problem of keeping a materialized view
up to date as underlying tables change. An alternative approach is to refetch all the data for a particular query
(with the appropriate authorization rules for the specific client). This is the approach we currently take.

Secondly, building a webserver to handle websockets in a scalable way is also sometimes a little hairy, but certain
frameworks and languages do make the concurrent programming required a little more tractable.

Refetching results for a GraphQL query
--------------------------------------

To understand why refetching a GraphQL query is hard, let’s look at how a GraphQL query is typically processed:

.. thumbnail:: ../../../../img/graphql/manual/subscriptions/graphql-resolvers.png
  :width: 70%
  :alt: graphql resolvers

The authorization + data fetching logic has to run for each “node” in the GraphQL query. This is scary, because even a
slightly large query fetching a collection could bring down the database quite easily. The N+1 query problem, also
common with badly implemented ORMs, is bad for your database and makes it hard to optimally query Postgres. Data loader
type patterns can alleviate the problem, but will still query the underlying Postgres database multiple times (reduces
to as many nodes in the GraphQL query from as many items in the response).

For live queries, this problem becomes worse, because each client’s query will translate into an independent refetch.
Even though the queries are the “same”, since the authorization rules create different session variables, independent
fetches are required for each client.

Hasura approach
---------------

Declarative mapping from the data models to the GraphQL schema is used to create a single SQL query to the database.
This avoids multiple hits to the database, whether there are a large number of items in the response or the number of
nodes in the GraphQL query are large.

.. toctree::
  :maxdepth: 1

  Idea #1: “Compile” a GraphQL query to a single SQL query <graphql-to-sql>
  Idea #2: Batch multiple live-queries into one SQL query <graphql-to-sql-multiplexed>
  Idea #3: Make authorization declarative <graphql-to-sql-with-authorization>

Benefits of this approach
^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura makes live-queries easy and accessible. The notion of queries is easily extended to live-queries without any
extra effort on the part of the developer using GraphQL queries. This is the most important thing for us.

1) Expressive/featureful live queries with full support for Postgres operators/aggregations/views/functions etc

2) Predictable performance

3) Vertical & Horizontal scaling

4) Works on all cloud/database vendors

**See:**

.. toctree::
  :maxdepth: 1

  refetch
