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

Currently, only functions which satisfy the following constraints can be exposed over the GraphQL API
(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Return type**: MUST be ``SETOF <table-name>``
- **Argument modes**: ONLY ``IN``

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
