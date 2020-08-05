.. meta::
   :description: Hasura Cloud read replicas
   :keywords: hasura, docs, cloud, read replicas, connections, pool

.. _read_replicas:

Read replicas
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura Cloud can load balance queries and subscriptions across read replicas while sending all mutations and metadata API calls to the master.

Adding read replica urls
------------------------

If you have configured your Postgres instances with replicas, the replica URLs can be added to Hasura using the following environment variable in your project ENV Vars tab:

.. code-block:: bash

   HASURA_GRAPHQL_READ_REPLICA_URLS=postgres://user:password@replica-host:5432/db

If you have multiple replicas, their urls can be added as comma separated values.

Connection pool parameters
--------------------------

Additional environment variables for connection pools, and for read replicas specifically:

``HASURA_GRAPHQL_PG_STRIPES``

``HASURA_GRAPHQL_PG_CONNECTIONS``

``HASURA_GRAPHQL_CONNECTIONS_PER_READ_REPLICA``

``HASURA_GRAPHQL_STRIPES_PER_READ_REPLICA``