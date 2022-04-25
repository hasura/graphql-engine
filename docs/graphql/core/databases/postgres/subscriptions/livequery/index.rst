.. meta::
   :description: Livequery subscriptions with Hasura
   :keywords: hasura, docs, subscription, livequery

.. _pg_live_query_subscriptions:

Postgres: Live query subscriptions
==================================

.. contents:: Table of contents
   :backlinks: none
   :depth: 1
   :local:

Introduction
------------

A Live query subscription will return the latest result of the query being made
and not necessarily all the individual events leading up to the result.

By default, updates are delivered to clients every **1 sec**.

See more details on :ref:`subscriptions execution and performance <pg_subscriptions_execution_and_performance>`.

Convert a query to a subscription
---------------------------------

You can turn any query into a live query subscription by simply replacing ``query`` with ``subscription`` as the operation type.

.. admonition:: Caveat

  Hasura follows the `GraphQL spec <https://graphql.github.io/graphql-spec/June2018/#sec-Single-root-field>`__ which
  allows for only one root field in a subscription.

Use cases
---------

- :ref:`pg_subscribe_field`
- :ref:`pg_subscribe_table`
- :ref:`pg_subscribe_derived`

.. toctree::
   :maxdepth: 1
   :hidden:

   Sample use cases <use-cases>
   Execution and performance <execution-and-performance>
