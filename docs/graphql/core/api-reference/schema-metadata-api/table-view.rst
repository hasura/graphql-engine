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

Introduction
------------

Track/untrack a table/view in Hasura GraphQL engine.

Only tracked tables/views are available for querying/mutating/subscribing data over the GraphQL API.

.. admonition:: Deprecation

  In versions ``v2.0.0`` and above, the schema/metadata API is deprecated in favour of the :ref:`schema API <schema_apis>` and the
  :ref:`metadata API <metadata_apis>`.

  Though for backwards compatibility, the schema/metadata APIs will continue to function.

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
              "insert_one":"AddAuthor",
              "update": "UpdateAuthors",
              "update_by_pk": "UpdateAuthor",
              "delete": "DeleteAuthors",
              "delete_by_pk": "DeleteAuthor"
           },
           "custom_column_names": {
              "id": "authorId"
           }
        }
      }
   }

A table can be tracked with a ``custom name``. This can be useful when a table
name is not GraphQL compliant, like ``Users Address``. A ``custom name`` like
``users_address`` will complement the ``"Users Address"``
table, so that it can be added to the GraphQL schema.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type": "track_table",
      "version": 2,
      "args": {
        "table": "Author Details",
        "configuration": {
           "custom_name": "author_details"
        }
      }
   }

The GraphQL nodes and typenames
that are generated will be according to the ``identifier``. For example, in this case,
the nodes generated will be:

- ``users_address``
- ``users_address_one``
- ``users_address_aggregate``
- ``insert_users_address``
- ``insert_users_address_one``
- ``update_users_address``
- ``update_users_address_by_pk``
- ``delete_users_address``
- ``delete_users_address_by_pk``

.. note::
  graphql-engine requires the constraint names (if any) of a table to be `GraphQL compliant <https://spec.graphql.org/June2018/#sec-Names>`__ in order to be able to track it.

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

.. _set_table_custom_fields:

set_table_custom_fields (deprecated)
------------------------------------

``set_table_custom_fields`` has been deprecated. Use the
:ref:`set_table_customization <set_table_customization>` API to set the custom
table fields.

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
           "insert_one":"AddAuthor",
           "update": "UpdateAuthors",
           "update_by_pk": "UpdateAuthor",
           "delete": "DeleteAuthors",
           "delete_by_pk": "DeleteAuthor"
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

.. _set_table_customization:

set_table_customization
-----------------------

``set_table_customization`` allows you to customize any given table with
a custom name, custom root fields and custom column names of an already tracked
table. This will **replace** the already present customization.

:ref:`set_table_custom_fields <set_table_custom_fields>` has been deprecated in
favour of this API.

Set the configuration for a table/view called ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type": "set_table_customization",
      "args": {
        "table": "author_details",
        "configuration": {
          "identifier": "author",
          "custom_root_fields": {
             "select": "Authors",
             "select_by_pk": "Author",
             "select_aggregate": "AuthorAggregate",
             "insert": "AddAuthors",
             "insert_one":"AddAuthor",
             "update": "UpdateAuthors",
             "update_by_pk": "UpdateAuthor",
             "delete": "DeleteAuthors",
             "delete_by_pk": "DeleteAuthor"
          },
          "custom_column_names": {
             "id": "authorId"
          }
        }
      }
   }

.. _set_table_customization_syntax:

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
     - :ref:`TableConfig <table_config>`
     - Configuration for the table/view

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
