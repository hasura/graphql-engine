.. meta::
  :description: Manage custom functions with the Hasura metadata API
  :keywords: hasura, docs, metadata API, API reference, custom function

.. _metadata_api_custom_functions:

Metadata API Reference: Custom Functions
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Track/untrack a custom SQL function in the Hasura GraphQL engine.

Only tracked custom functions are available for querying/mutating/subscribing data over the GraphQL API.

.. TODO: add other existing APIs

.. _pg_create_function_permission:

pg_create_function_permission
-----------------------------

``pg_create_function_permission`` is used to add permission to an existing custom function.
To add a function permission, the graphql-engine should have disabled inferring of
function permissions and the provided role should have select permissions to the
target table of the function.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_function_permission",
       "args": {
          "function": "get_articles",
          "role": "user"
       }
   }

.. _pg_create_function_permission_args_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - function
     - true
     - :ref:`FunctionName <FunctionName>`
     - Name of the SQL function
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Name of the role
   * - source
     - false
     - Text
     - Name of the source of the SQL function

.. _pg_drop_function_permission:

pg_drop_function_permission
---------------------------

``pg_drop_function_permission`` is used to drop an existing function permission.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_drop_function_permission",
       "args": {
          "function": "get_articles",
          "role": "user"
       }
   }

.. _pg_drop_function_permission_args_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - function
     - true
     - :ref:`FunctionName <FunctionName>`
     - Name of the SQL function
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Name of the role
   * - source
     - false
     - Text
     - Name of the source of the SQL function
