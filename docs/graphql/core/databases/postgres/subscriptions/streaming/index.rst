.. meta::
   :description: Streaming subscriptions with Hasura
   :keywords: hasura, docs, subscription, streaming

.. _pg_streaming_subscriptions:

Postgres: Streaming subscriptions
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Introduction
------------

A streaming subscription streams the response according to the cursor provided
by the user while making the subscription. Streaming subscriptions can be used to
subscribe only to the data which has been newly added to the result set.

.. admonition:: Supported from

   Streaming subscriptions are supported in Hasura GraphQL engine versions ``v2.5.0-beta.1`` and above.

.. admonition:: Enabling streaming subscriptions

   Streaming subscriptions in the graphql-engine are disabled by default. To enable it,
   set the environment variable ``HASURA_GRAPHQL_EXPERIMENTAL_FEATURES`` to ``streaming_subscriptions``.

How it works?
-------------

In streaming subscriptions, the server maintains a cursor value with a subscription and
after streaming each batch, the value of the cursor is updated. Ideally, the cursor chosen
should represent unique and sortable values so that each row is sent exactly once to a subscriber.

Streaming subscriptions work well with other Hasura features like
:ref:`permissions <permission_rules>` and :ref:`relationships <pg_table_relationships>` and also
leverage the power of :ref:`multiplexing <subscription_multiplexing>`.

.. note::

   In the case of streaming subscriptions, the multiplexed batch size can be configured via
   ``HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE`` and the refetch interval can be
   configured via ``HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL``.

Use cases
---------

- :ref:`pg_streaming_subscriptions_use_cases`

.. toctree::
   :maxdepth: 1
   :hidden:

   Sample use cases <use-cases>
