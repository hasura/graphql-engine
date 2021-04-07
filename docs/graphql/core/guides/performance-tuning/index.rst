.. meta::
    :description: Tuning Hasura for High-performance Subscriptions
    :keywords: hasura, docs, subscriptions
 
.. _subscriptions_performance_tuning:

.. include:: <isonum.txt>

Tuning Subscriptions
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This doc explains how subscriptions work in Hasura and the factors/parameters to be considered for performance tuning of Hasura for subscriptions.

Execution
---------

The Hasura GraphQL engine subscriptions are actually live queries, where queries are made to the DB at specific intervals and the latest result of the query is returned to the client. 
The interval can be configured via the ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL`` env var or the ``--live-queries-multiplexed-refetch-interval`` flag. 
By default subscribers get updated every 1 sec.

.. thumbnail:: /img/graphql/core/guides/subscription-model.png
    :class: no-shadow
    :width: 900px
    :alt: Hasura subscription example setup

An example of subscription for tracking the live location of the delivery_agent carrying the food-order in a food-ordering system.

The Hasura GraphQL engine optimises subscriptions by combining multiple live-queries(subscriptions) into one SQL query to the DB. 
This is called multiplexing of live queries. Let's consider a food-ordering system where multiple active clients running subscriptions to get the latest order status and location of the delivery_agent carrying their food-orders. 
Instead of making a query to the DB at specific intervals for each of the clients, Hasura makes a single SQL query to the DB having a “JOIN-relation” between the query variables and the query itself, and gets a single response. 
Each row in the response contains the result for each user. Once Hasura gets the response from the DB, it diff-checks the response and returns the result to the client only if there is any difference. 
For the food-ordering system, the query-variables(orderIDs and userIDs) and the SQL query are given in the diagram.

.. thumbnail:: /img/graphql/core/guides/subscription-multiplexing.png
    :class: no-shadow
    :width: 900px
    :alt: Hasura subscription multiplexing AST

Also the multiplexed live queries are split into batches which can be configured via ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE`` env or the ``--live-queries-multiplexed-batch-size`` flag. 
By default the ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE`` is set to 100. For example, if there are 1000 subscription clients with default ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE``, 
Hasura multiplexes 100 subscriptions into 1 batch and make a single SQL query to the DB for that batch. 
So, in total there will be only 10 SQL queries to the DB for 1000 subscriptions.

Improving subscription performance
----------------------------------

Depending on the complexity of the subscription query and how realtime the subscription data should be, one can improve the overall subscription performance of the system by tuning the 
``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE``(batch_size) and ``HASURA_GRAPHQL_PG_CONNECTIONS`` (connection_pool) for subscriptions.

For relatively simple subscription queries (|uARR| - parameter increases, |dArr| - parameter decreases)

.. list-table::
   :header-rows: 1

   * - 
     - DB CPU utilization
     - DB Memory utilization
     - Number of DB connections
     - Hasura CPU utilization
     - Hasura Memory utilization
   * - batch_size |uArr|
     - |dARR|
     - |dARR|
     - |dARR|
     - no change
     - no change
   * - connection_pool |uArr|
     - |uArr|
     - |uArr|
     - |uArr|
     - no change
     - no change
   * - refetch_interval |uArr|
     - |dARR|
     - |dARR|
     - no change
     - no changed
     - no change

The above trends are indicative in nature of the general trend. 
As Hasura resource utilisation is not the bottleneck, to improve subscription performance, one should look for DB performance improvement with different combinations of the above parameters.

However when the subscription queries are complex, finding the optimal parameter combination at scale varies from case to case. 
Below are few pointers one can follow to improve subscription performance.

1. Increase the refetch_interval to make sure the refetch_interval is greater than the DB execution time for a batch of subscriptions
2. For lower subscription concurrency(say 1000), lowering batch_size tends to improve subscription latency. One should consider setting ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE`` = 1, and see if there is improvement in performance
3. For higher concurrency in subscription(5000 or more), higher ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE`` value helps improve latency. But this is not a linear correlation, as there tends to be a particular value of batch_size which gives the best subscription performance, beyond which performance decreases