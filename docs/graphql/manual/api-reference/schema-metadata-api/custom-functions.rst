Schema/Metadata API Reference: Custom Functions
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Add or remove a custom SQL function to Hasura GraphQL Engine's metadata using following API.

.. Note::

   Only custom functions added to metadata are available for ``querying/subscribing`` data over **GraphQL** API.

.. _track_function:

track_function
--------------

``track_function`` is used to add a custom SQL function.

Refer :ref:`this <supported_sql_functions>` for constraints on supported functions.

Add a SQL function ``search_articles``:

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

``untrack_function`` is used to remove a SQL function from metadata.

Remove a SQL function ``search_articles``:

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
