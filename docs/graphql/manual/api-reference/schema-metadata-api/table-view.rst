Schema/Metadata API Reference: Tables/Views
============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Add or remove a table/view to Hasura GraphQL Engine's metadata using following API.

.. Note::

   Only tables/views added to metadata are available for ``querying/mutating/subscribing`` data over **GraphQL** API.

.. _track_table:

track_table
-----------

``track_table`` is used to add a table/view.

Add a table/view ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "track_table",
       "args": {
           "schema": "public",
           "name": "author"
       }
   }

.. _track_table_syntax:

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
     - :ref:`TableName <TableName>`
     - Name of the table

.. _untrack_table:

untrack_table
-------------

``untrack_table`` is used to remove a table/view. 

Remove a table/view ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "untrack_table",
       "args": {
           "table": {
               "schema": "public",
               "name": "author"
            },
           "cascade": true
       }
   }

    
.. _untrack_table_syntax:

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
     - :ref:`TableName <TableName>`
     - Name of the table
   * - cascade
     - false
     - Boolean
     - When set to ``true``, the effect (if possible) is cascaded to any metadata dependent objects (relationships, permissions, templates).
