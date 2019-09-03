Schema/Metadata API Reference: Tables/Views
===========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Track/untrack a table/view in Hasura GraphQL engine

Only tracked tables/views are available for querying/mutating/subscribing data over the GraphQL API.

.. _track_table:

track_table
-----------

``track_table`` is used to add a table/view to the GraphQL schema.

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
   * - is_enum
     - false
     - Boolean
     - When set to ``true``, creates the table as an :ref:`enum table <create_enum_table>`.

.. _set_table_is_enum:

set_table_is_enum
-----------------

``set_table_is_enum`` sets whether an already-tracked table should be used as an :ref:`enum table <create_enum_table>`.

Use table ``user_role`` as an enum table:

.. code-block:: http

  POST /v1/query HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "set_table_is_enum",
    "args": {
      "table": {
        "schema": "public",
        "name": "user_role"
      },
      "is_enum": true
    }
  }

.. _set_table_is_enum_syntax:

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
   * - is_enum
     - true
     - Boolean
     - Whether or not the table should be used as an :ref:`enum table <enum table>`.

.. _untrack_table:

untrack_table
-------------

``untrack_table`` is used to remove a table/view from the GraphQL schema.

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
