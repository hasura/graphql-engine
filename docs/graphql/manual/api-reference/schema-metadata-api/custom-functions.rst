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

Track/untrack a custom SQL function in the Hasura GraphQL engine.

Only tracked custom functions are available for querying/mutating/subscribing data over the GraphQL API.

.. _track_function:

track_function
--------------

``track_function`` is used to add a custom SQL function to the GraphQL schema.
Also refer a note :ref:`here <note>`.

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

Version 2 of ``track_function`` is used to add a custom SQL function to the GraphQL schema with configuration.
Also refer a note :ref:`here <note>`.

Add an SQL function called ``search_articles`` with a Hasura session argument.

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

.. _function_configuration:

Function Configuration
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - session_argument
     - false
     - `String`
     - Function argument which accepts session info JSON

.. _note:

.. note::

   Currently, only functions which satisfy the following constraints can be exposed over the GraphQL API
   (*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

   - **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
   - **Return type**: MUST be ``SETOF <table-name>``
   - **Argument modes**: ONLY ``IN``

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
