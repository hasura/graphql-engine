.. meta::
   :description: Manage databases with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, database, source

.. _metadata_api_sources:

Metadata API Reference: Databases (v1.4 and above)
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Add/remove databases in Hasura GraphQL engine.

.. _pg_add_source:

pg_add_source
-------------

``pg_add_source`` is used to connect a Postgres database to Hasura.

Add a database with name ``pg1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "pg_add_source",
    "args": {
      "name": "pg1",
      "configuration": {
        "connection_info": {
          "database_url": "<db-connection-string>",
          "pool_settings": {
            "max_connections": 50,
            "idle_timeout": 180,
            "retries": 1
          }
        }
      }
    }
  }

.. _pg_add_source_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`SourceName <SourceName>`
     - Name of the Postgres database
   * - configuration
     - true
     - :ref:`PGConfiguration <PGConfiguration>`
     - Database connection configuration

.. _pg_drop_source:

pg_drop_source
--------------

``pg_drop_source`` is used to remove a Postgres database from Hasura.

Remove a database with name ``pg1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "pg_drop_source",
    "args": {
      "name": "pg1"
    }
  }

.. _pg_drop_source_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`SourceName <SourceName>`
     - Name of the Postgres database

