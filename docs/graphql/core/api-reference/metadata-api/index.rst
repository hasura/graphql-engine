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

The metadata API is primarily intended to be used as an ``admin`` API to manage the Hasura metadata.

.. admonition:: Supported from

  The metadata API is supported for versions ``v.1.4.0`` and above.


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
      "resource_version": <Integer> (optional),
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
   * - resource_version
     - false
     - Integer
     - Version of the resource that you are targeting for replacement
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

   * - :ref:`export_metadata`
     - :ref:`export_metadata_examples`
     - 2
     - Export existing metadata with resource version included.

   * - :ref:`replace_metadata`
     - :ref:`replace_metadata_examples`
     - 2
     - Replace existing metadata with check against current resource_version.

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


.. _metadata_resource_version:

Metadata Resource Versioning
----------------------------

Metadata is versioned with an optional ``resource_version`` field in operations and responses.

This is intended to allow for feedback when replacing metadata with modifications to an out-of-date copy.

The ``resource_version`` supplied must match the version returned otherwise a 409 error response is returned.

The version is incremented on any operation that modified metadata as well as ``reload_metadata``.



.. _export_metadata_examples:

Export Metadata Examples
------------------------

``export_metadata`` is used to export the current metadata from the server as a JSON file.

V1 Example: See :ref:`export_metadata`

V2 Example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
        "type": "export_metadata",
        "version": 2,
        "args": {}
   }

Response:

.. code-block:: json

    {
    "resource_version": 8,
    "metadata": {
        "version": 3,
        "sources": [
        {
            "name": "default",
            "tables": [
            {
                "table": {

.. _replace_metadata_examples:

Replace Metadata Examples
-------------------------

``replace_metadata`` is used to replace the current metadata with a JSON object.

V1 Example: See :ref:`replace_metadata_syntax_v1`

Version 2 example with inconsistencies and allow_inconsistent_metadata=true:

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

Version 2 example with invalid ``resource_version``:

    HTTP/1.1 409 Conflict

.. code-block:: json

    {
      "path": "$",
      "error": "metadata resource version referenced (2) did not match current version",
      "code": "conflict"
    }
