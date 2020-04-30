.. meta::
   :description: Manage tables and views with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, table, view

.. _api_tables_views:

Schema/Metadata API Reference: Tables/Views
===========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Track/untrack a table/view in Hasura GraphQL engine.

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

.. _track_table_v2:

track_table v2
--------------

Version 2 of ``track_table`` is used to add a table/view to the GraphQL schema with configuration. You can customise the root field names.

Add a table/view ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type": "track_table",
      "version": 2,
      "args": {
        "table": "author",
        "configuration": {
           "custom_root_fields": {
              "select": "Authors",
              "select_by_pk": "Author",
              "select_aggregate": "AuthorAggregate",
              "insert": "AddAuthors",
              "update": "UpdateAuthors",
              "delete": "DeleteAuthors"
           },
           "custom_column_names": {
              "id": "authorId"
           }
        }
      }
   }

.. _track_table_args_syntax_v2:

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
   * - configuration
     - false
     - :ref:`Table Config <table_config>`
     - Configuration for the table/view

.. _table_config:

Table Config
^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - custom_root_fields
     - false
     - :ref:`Custom Root Fields <custom_root_fields>`
     - Customise the root fields
   * - custom_column_names
     - false
     - :ref:`CustomColumnNames`
     - Customise the column fields

.. _custom_root_fields:

Custom Root Fields
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - select
     - false
     - `String`
     - Customise the ``<table-name>`` root field
   * - select_by_pk
     - false
     - `String`
     - Customise the ``<table-name>_by_pk`` root field
   * - select_aggregate
     - false
     - `String`
     - Customise the ``<table-name>_aggregete`` root field
   * - insert
     - false
     - `String`
     - Customise the ``insert_<table-name>`` root field
   * - insert_one
     - false
     - `String`
     - Customise the ``insert_<table-name>_one`` root field
   * - update
     - false
     - `String`
     - Customise the ``update_<table-name>`` root field
   * - update_by_pk
     - false
     - `String`
     - Customise the ``update_<table-name>_by_pk`` root field
   * - delete
     - false
     - `String`
     - Customise the ``delete_<table-name>`` root field
   * - delete_by_pk
     - false
     - `String`
     - Customise the ``delete_<table-name>_by_pk`` root field

.. _set_table_custom_fields:

set_table_custom_fields
-----------------------

``set_table_custom_fields`` in version ``2`` sets the custom root fields and
custom column names of already tracked table. This will **replace** the already
present custom fields configuration.

Set custom fields for table/view ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type": "set_table_custom_fields",
      "version": 2,
      "args": {
        "table": "author",
        "custom_root_fields": {
           "select": "Authors",
           "select_by_pk": "Author",
           "select_aggregate": "AuthorAggregate",
           "insert": "AddAuthors",
           "update": "UpdateAuthors",
           "delete": "DeleteAuthors"
        },
        "custom_column_names": {
           "id": "authorId"
        }
      }
   }

.. _set_table_custom_fields_args_syntax:

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
   * - custom_root_fields
     - false
     - :ref:`Custom Root Fields <custom_root_fields>`
     - Customise the root fields
   * - custom_column_names
     - false
     - :ref:`CustomColumnNames`
     - Customise the column fields


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
     - When set to ``true``, the effect (if possible) is cascaded to any metadata dependent objects (relationships, permissions, templates)
