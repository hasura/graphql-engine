.. meta::
   :description: Hasura schema API reference
   :keywords: hasura, docs, schema API, API reference

.. _schema_apis:

Schema API Reference (v2.0 and above)
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The schema API provides the following features:

1. Execute SQL on the underlying Postgres database, supports schema modifying actions.

This is primarily intended to be used as an ``admin`` API to manage the Hasura schema.

.. admonition:: Supported from

  The schema API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

Endpoint
--------

All requests are ``POST`` requests to the ``/v2/query`` endpoint.

Request structure
-----------------

.. code-block:: http

   POST /v2/query HTTP/1.1

   {
      "type": "<query-type>",
      "args": <args-object>
   }

Request body
^^^^^^^^^^^^

.. parsed-literal::

   :ref:`Query <schema_query>`

.. _schema_query:

Query
*****

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - type
     - true
     - String
     - Type of the query
   * - args
     - true
     - JSON Value
     - The arguments to the query
   * - version
     - false
     - Integer
     - Version of the API (default: 1)

Request types
-------------

The various types of queries are listed in the following table:

.. list-table::
   :header-rows: 1

   * - ``type``
     - ``args``
     - ``version``
     - Synopsis

   * - **bulk**
     - :ref:`Query <schema_query>` array
     - 1
     - Execute multiple operations in a single query

   * - :ref:`schema_run_sql`
     - :ref:`run_sql_args <schema_run_sql_syntax>`
     - 1
     - Run SQL directly on Postgres

Response structure
------------------

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Description
     - Response structure

   * - ``200``
     - Success
     - .. parsed-literal::

        Request specific

   * - ``400``
     - Bad request
     - .. code-block:: haskell

          {
              "path"  : String,
              "error" : String
          }

   * - ``401``
     - Unauthorized
     - .. code-block:: haskell

          {
              "error" : String
          }

   * - ``500``
     - Internal server error
     - .. code-block:: haskell

          {
              "error" : String
          }

Disabling schema API
--------------------

Since this API can be used to make changes to the GraphQL schema, it can be
disabled, especially in production deployments.

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be used to
enable/disable this API. By default, the schema/metadata API is enabled. To disable it, you need
to explicitly state that this API is not enabled i.e. remove it from the list of enabled APIs.

.. code-block:: bash

   # enable only graphql api, disable metadata and pgdump
   --enabled-apis="graphql"
   HASURA_GRAPHQL_ENABLED_APIS="graphql"

See :ref:`server_flag_reference` for info on setting the above flag/env var.

.. toctree::
   :maxdepth: 1
   :hidden:

   Run SQL <run-sql>