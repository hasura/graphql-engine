.. meta::
   :description: Manage custom functions with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, custom function

.. _api_custom_functions:

Schema/Metadata API Reference: Custom Functions
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Track/untrack a custom SQL function in the Hasura GraphQL engine.

Only tracked custom functions are available for querying/mutating/subscribing data over the GraphQL API.

.. admonition:: Deprecation

  In versions ``v2.0.0`` and above, the schema/metadata API is deprecated in favour of the :ref:`schema API <schema_apis>` and the
  :ref:`metadata API <metadata_apis>`.

  Though for backwards compatibility, the schema/metadata APIs will continue to function.

.. _track_function:

track_function
--------------

``track_function`` is used to add a custom SQL function to the ``query`` root field of the GraphQL schema.
Also refer a note :ref:`here <function_req_note>`.

Add an SQL function ``search_articles``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "track_function",
       "args": {
           "schema": "public",
           "name": "search_articles"
       }
   }

.. _track_function_v2:

track_function v2
-----------------

Version 2 of ``track_function`` is used to add a custom SQL function to the GraphQL schema.
It supports more configuration options than v1, and also supports tracking
functions as mutations.
Also refer a note :ref:`here <function_req_note>`.

Track an SQL function called ``search_articles`` with a Hasura session argument:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "track_function",
       "version": 2,
       "args": {
           "function": {
               "schema": "public",
               "name": "search_articles"
           },
           "configuration": {
               "session_argument": "hasura_session"
           }
       }
   }

Track ``VOLATILE`` SQL function ``reset_widget`` as a mutation, so it appears
as a top-level field under the ``mutation`` root field:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "track_function",
       "version": 2,
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

.. _track_function_args_syntax_v2:

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

.. _untrack_function:

untrack_function
----------------

``untrack_function`` is used to remove a SQL function from the GraphQL schema.

Remove an SQL function ``search_articles``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "untrack_function",
       "args": {
           "schema": "public",
           "name": "search_articles"
       }
   }

.. _args_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`FunctionName <FunctionName>`
     - Name of the SQL function
