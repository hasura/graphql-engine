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

Hasura Cloud can load balance queries and subscriptions across read replicas while sending all mutations and metadata API calls to the primary.

Adding read replica urls
------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

     Currently it is only possible to add read replicas for a database at the time of connecting.

     Head to ``Data -> Manage -> Connect database``

     .. thumbnail:: /img/graphql/cloud/read-replicas/connect-db-with-replica.png
        :alt: Connect database with read replica
        :width: 1000px

   .. tab:: CLI

      You can add read replicas for a database by adding their config to the ``/metadata/databases/database.yaml`` file:

      .. code-block:: yaml
         :emphasize-lines: 11-17

         - name: <db-name>
           kind: postgres
           configuration:
             connection_info:
               database_url:
                 from_env: <DATABASE_URL_ENV>
               pool_settings:
                 idle_timeout: 180
                 max_connections: 50
                 retries: 1
             read_replicas:
             - database_url:
                 from_env: <DATABASE_REPLICA_URL_ENV>
               pool_settings:
                 idle_timeout: 180
                 max_connections: 50
                 retries: 1

      Apply the metadata by running:

      .. code-block:: yaml

         hasura metadata apply

   .. tab:: API

      Currently it is only possible to add read replicas for a database at the time of connecting using the :ref:`pg_add_source metadata API <pg_add_source>`

      .. code-block:: http
         :emphasize-lines: 15-26

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
           "type": "pg_add_source",
           "args": {
             "name": "<db_name>",
             "configuration": {
               "connection_info": {
                 "database_url": {
                   "from_env": "<DATABASE_URL_ENV>"
                 }
               },
             "read_replicas": [
               {
                 "database_url": {
                   "from_env": "<DATABASE_REPLICA_URL_ENV>"
                 },
                 "pool_settings": {
                   "retries": 1,
                   "idle_timeout": 180,
                   "max_connections": 50
                 }
               }
             ]
           }
         }

.. admonition:: For existing v1.3 projects

   If you have configured your Postgres instances with replicas, the replica URLs can be added to Hasura using the following environment variable in your project ENV Vars tab:

   .. code-block:: bash

      HASURA_GRAPHQL_READ_REPLICA_URLS=postgres://user:password@replica-host:5432/db

   If you have multiple replicas, their urls can be added as comma separated values.

   Additional environment variables for read replicas specifically:

   ``HASURA_GRAPHQL_CONNECTIONS_PER_READ_REPLICA``

   ``HASURA_GRAPHQL_STRIPES_PER_READ_REPLICA``
