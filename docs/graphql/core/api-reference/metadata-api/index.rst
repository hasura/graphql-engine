.. meta::
   :description: Hasura metadata API reference
   :keywords: hasura, docs, metadata API, API reference

.. _metadata_apis:

Metadata API Reference (v1.4 and above)
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This is primarily intended to be used as an ``admin`` API to manage the Hasura metadata.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/metadata`` endpoint.

Request structure
-----------------

.. code-block:: http

   POST /v1/metadata HTTP/1.1

   {
      "type": <query-type>,
      "version": <Integer> (optional),
      "args": <args-object>
   }

The structure of args depends on the type and version.

Request body
^^^^^^^^^^^^

.. parsed-literal::

   Query_

.. _Query:

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
     - Version of the API (inferred by args structure)

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
     - :ref:`Query <Query>` array
     - 1
     - Execute multiple operations in a single query

   * - :ref:`pg_create_function_permission`
     - :ref:`pg_create_function_permission_args <pg_create_function_permission_args_syntax>`
     - 1
     - Create a function permission

   * - :ref:`pg_drop_function_permission`
     - :ref:`pg_drop_function_permission_args <pg_drop_function_permission_args_syntax>`
     - 1
     - Drop an existing function permission

   * - :ref:`replace_metadata_v2`
     - :ref:`replace_metadata_syntax_v2`
     - 1
     - Drop an existing function permission

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

Disabling metadata API
----------------------

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

Error codes
-----------

.. csv-table::
   :file: dataerrors.csv
   :widths: 10, 20, 70
   :header-rows: 1

.. toctree::
   :maxdepth: 1
   :hidden:

    Custom Functions <custom-functions>
    Manage Metadata <manage-metadata>
