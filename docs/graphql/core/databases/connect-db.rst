.. meta::
  :description: Connect a database to Hasura
  :keywords: hasura, docs, databases, connect

.. _connect_database:

Connecting databases to Hasura GraphQL engine
=============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

From ``v2.0.0`` onwards Hasura GraphQL engine allows connecting to multiple databases and build a unified GraphQL API based on the
database schemas.

.. _connect_database_v2.0:

Connect a database
------------------

In versions ``v2.0.0`` and above, databases can be connected and removed dynamically without having to restart the GraphQL
engine instance via the console / metadata APIs / CLI:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Head to ``Data -> Manage -> Connect database``

      .. thumbnail:: /img/graphql/core/databases/connect-default-db.png
         :alt: Connect default database
         :width: 1000px


   .. tab:: CLI

      In your ``config v3`` project, head to the ``/metadata/databases/databases.yaml`` file and add the database
      configuration as below:

      .. code-block:: yaml

         - name: <db_name>
           configuration:
             connection_info:
               database_url:
                 from_env: <DB_URL_ENV_VAR>
               pool_settings:
                 idle_timeout: 180
                 max_connections: 50
                 retries: 1
             tables: []
             functions: []

      Apply the metadata by running:

      .. code-block:: bash

        hasura metadata apply


   .. tab:: API

      Depending on the type of database, you can add a database using the :ref:`sources metadata API <metadata_api_sources>`.

      For example:

      .. code-block:: http

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
                   "from_env": "<DB_URL_ENV_VAR>"
                 },
                 "pool_settings": {
                   "retries": 1,
                   "idle_timeout": 180,
                   "max_connections": 50
                 }
               }
             }
           }
         }

.. note::

  - You can connect to databases either using env vars or by using their raw connection string/parameters. It is
    recommended to use env vars for better security *(as connection details are part of Hasura metadata)* as well as
    to allow configuring different databases in different environments *(like staging/production)* easily.

  - A Postgres database can be connected using the ``HASURA_GRAPHQL_DATABASE_URL`` env var as well in which case it gets
    added automatically as a database named ``default``