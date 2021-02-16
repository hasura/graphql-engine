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

   * - :ref:`replace_metadata_example`
     - :ref:`replace_metadata_example_args`
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


.. _replace_metadata_example:

replace_metadata
----------------

``replace_metadata`` is used to replace/import metadata into Hasura. Existing
metadata will be replaced with the new one.

.. code-block:: http

    POST /v1/query HTTP/1.1
    Content-Type: application/json
    X-Hasura-Role: admin

    {
        "type" : "replace_metadata",
        "version": 1 | 2
        "args": <replace-metadata-args>
    }

For version 1, this API corresponds to the legacy API documented under  :ref:`replace_metadata` in the ``/v1/query`` endpoint.

.. _replace_metadata_example_args:

Args syntax
^^^^^^^^^^^

For version 2, the following structure is used:

.. code-block:: json

    {
        allow_inconsistent_metadata: Boolean
        metadata: metadata-object
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - allow_inconsistent_metadata
     - false
     - Boolean
     - If set to ``true``, metadata will be replaced with a warning in the response indicating which items are inconsistent (default: ``false``)
   * - metadata
     - true
     - :ref:`export_metadata`
     - The metadata that will replace the current metadata.

If the version is not specified, then it is inferred from the format of ``args``.

Request
^^^^^^^

.. code-block:: http

    POST /v1/metadata HTTP/1.1
    Content-Type: application/json
    X-Hasura-Role: admin

    {
        "type" : "replace_metadata",
        "version": 2
        "args": {
          "allow_inconsistent_metadata": Boolean,
          "metadata": <metadata-object>
        }
    }

Responses
^^^^^^^^^

Version 2 with inconsistencies and allow_inconsistent_metadata=false, or omitted corresponds with the response document in  :ref:`replace_metadata`.

Version 2 example with inconsistencies and allow_inconsistent_metadata=true includes an ``is_consistent`` and ``inconsistent_objects`` coresponding to :ref:`get_inconsistent_metadata`.

    HTTP/1.1 200 OK

.. code-block:: json

    {
    "is_consistent": false,
    "inconsistent_objects": [
        {
        "definition": {
            "definition": {
            "url": "http://localhost:5000/hello-graphql",
            "forward_client_headers": false
            },
            "name": "test",
            "permissions": [],
            "comment": "testing replace metadata with remote schemas"
        },
        "reason": "HTTP exception occurred while sending the request to http://localhost:5000/hello-graphql",
        "type": "remote_schema"
        }, ...


Error codes
-----------

.. csv-table::
   :file: dataerrors.csv
   :widths: 10, 20, 70
   :header-rows: 1
