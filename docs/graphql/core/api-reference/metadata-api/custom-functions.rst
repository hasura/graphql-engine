.. meta::
  :description: Manage custom functions with the Hasura metadata API
  :keywords: hasura, docs, metadata API, API reference, custom function

.. _metadata_api_custom_functions:

Metadata API Reference: Custom Functions (v2.0 and above)
=========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Track/untrack a custom SQL function in the Hasura GraphQL engine.

Only tracked custom functions are available for querying/mutating/subscribing data over the GraphQL API.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _pg_track_function:

pg_track_function
-----------------

``pg_track_function`` is used to add a custom SQL function to the GraphQL schema.
It supports more configuration options than v1, and also supports tracking
functions as mutations.
Also refer a note :ref:`here <function_req_note>`.

Track an SQL function called ``search_articles`` with a Hasura session argument:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_track_function",
       "args": {
           "function": {
               "schema": "public",
               "name": "search_articles"
           },
           "source": "default",
           "configuration": {
               "session_argument": "hasura_session"
           }
       }
   }

Track ``VOLATILE`` SQL function ``reset_widget`` as a mutation, so it appears
as a top-level field under the ``mutation`` root field:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_track_function",
       "args": {
           "function": {
               "schema": "public",
               "name": "reset_widget"
           },
           "configuration": {
               "exposed_as": "mutation"
           }
       }
   }

If ``exposed_as`` is omitted, the location in the schema to expose the function
will be inferred from the function's volatility, with ``VOLATILE`` functions
appearing under the ``mutation`` root, and others ending up under
``query/subscription``.

In most cases you will want ``VOLATILE`` functions to only be exposed as
mutations, and only ``STABLE`` and ``IMMUTABLE`` functions to be queries.
When tracking ``VOLATILE`` functions under the ``query`` root, the user needs
to guarantee that the field is idempotent and side-effect free, in the context
of the resulting GraphQL API.

One such use case might be a function that wraps a simple query and performs
some logging visible only to administrators.

.. note::

   It's easy to accidentally give an SQL function the wrong volatility (or for a
   function to end up with ``VOLATILE`` mistakenly, since it's the default).

.. _pg_track_function_syntax:

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
   * - configuration
     - false
     - :ref:`Function Configuration <function_configuration>`
     - Configuration for the SQL function
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the function (default: ``default``)

.. _pg_untrack_function:

pg_untrack_function
-------------------

``pg_untrack_function`` is used to remove a SQL function from the GraphQL schema.

Remove an SQL function ``search_articles``:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_untrack_function",
       "args": {
           "function": {
              "schema": "public",
              "name": "search_articles"
           },
           "source": "default"
       }
   }

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
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the function (default: ``default``)

.. _pg_create_function_permission:

pg_create_function_permission
-----------------------------

``pg_create_function_permission`` is used to add permission to an existing custom function.
To add a function permission, the provided role should have select permissions to the
target table of the function.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_function_permission",
       "args": {
          "function": "get_articles",
          "source": "default",
          "role": "user"
       }
   }

.. _pg_create_function_permission_syntax:

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
     - Name of the source database of the function (default: ``default``)

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
          "role": "user",
          "source": "default"
       }
   }

.. _pg_drop_function_permission_syntax:

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
     - Name of the source database of the function (default: ``default``)
