.. meta::
   :description: Manage databases with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, database, source

.. _metadata_api_sources:

Metadata API Reference: Databases (v2.0 and above)
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Add/remove databases in Hasura GraphQL engine.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

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
          "database_url": {
             "from_env": "<DB_URL_ENV_VAR>"
           },
          "pool_settings": {
            "max_connections": 50,
            "idle_timeout": 180,
            "retries": 1,
            "pool_timeout": 360,
            "connection_lifetime": 600
          },
          "use_prepared_statements": true,
          "isolation_level": "read-committed",
        }
      },
      "replace_configuration": false
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
   * - replace_configuration
     - false
     - Boolean
     - If set to ``true`` the configuration will be replaced if the source with
       given name already exists (default: ``false``)

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
      "name": "pg1",
      "cascade": true
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
   * - cascade
     - false
     - Boolean
     - When set to ``true``, the effect (if possible) is cascaded to any metadata dependent objects (relationships, permissions etc.) from other sources (default: ``false``)

.. _rename_source:

rename_source
-------------

``rename_source`` is used to rename an existing source.

Given there already exists a database with name ``pg1``, we can rename it to ``pg2`` using:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "rename_source",
    "args": {
      "name": "pg1",
      "new_name": "pg2"
    }
  }

Note that all settings are kept, only the name is changed.

.. _rename_source_syntax:

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
     - Name of the database
   * - new_name
     - true
     - :ref:`SourceName <SourceName>`
     - Name of the database

.. _mssql_add_source:

mssql_add_source
----------------

``mssql_add_source`` is used to connect an MS SQL Server database to Hasura.

Add a database with name ``mssql1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "mssql_add_source",
    "args": {
      "name": "mssql1",
      "configuration": {
        "connection_info": {
          "connection_string": {
             "from_env": "<CONN_STRING_ENV_VAR>"
           },
          "pool_settings": {
            "max_connections": 50,
            "idle_timeout": 180
          }
        }
      }
    }
  }

.. _mssql_add_source_syntax:

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
     - Name of the MS SQL Server database
   * - configuration
     - true
     - :ref:`MsSQLConfiguration <MsSQLConfiguration>`
     - Database connection configuration
   * - replace_configuration
     - false
     - Boolean
     - If set to ``true`` the configuration will be replaced if the source with
       given name already exists (default: ``false``)

.. _mssql_drop_source:

mssql_drop_source
-----------------

``mssql_drop_source`` is used to remove an MS SQL Server database from Hasura.

Remove a database with name ``mssql1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "mssql_drop_source",
    "args": {
      "name": "mssql1"
    }
  }

.. _mssql_drop_source_syntax:

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
     - Name of the MS SQL Server database
   * - cascade
     - false
     - Boolean
     - When set to ``true``, the effect (if possible) is cascaded to any metadata dependent objects (relationships, permissions etc.) from other sources (default: ``false``)


.. _bigquery_add_source:

bigquery_add_source
-------------------

``bigquery_add_source`` is used to connect a BigQuery database to Hasura.

Add a database with name ``bigquery1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "bigquery_add_source",
    "args": {
      "name": "bigquery1",
      "configuration": {
        "service_account": "bigquery_service_account",
        "project_id": "bigquery_project_id",
        "datasets": "dataset1, dataset2"
      }
    }
  }

.. _bigquery_add_source_syntax:

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
     - Name of the BigQuery database
   * - configuration
     - true
     - :ref:`BigQueryConfiguration <BigQueryConfiguration>`
     - Database connection configuration
   * - replace_configuration
     - false
     - Boolean
     - If set to ``true`` the configuration will be replaced if the source with
       given name already exists (default: ``false``)

.. _bigquery_drop_source:

bigquery_drop_source
--------------------

``bigquery_drop_source`` is used to remove a BigQuery database from Hasura.

Remove a database with name ``bigquery1``:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "bigquery_drop_source",
    "args": {
      "name": "bigquery1"
    }
  }

.. _bigquery_drop_source_syntax:

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
     - Name of the BigQuery database
   * - cascade
     - false
     - Boolean
     - When set to ``true``, the effect (if possible) is cascaded to any metadata dependent objects (relationships, permissions etc.) from other sources (default: ``false``)
