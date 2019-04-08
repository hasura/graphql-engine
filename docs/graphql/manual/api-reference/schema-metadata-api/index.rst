.. _metadata_apis:

Schema / Metadata API Reference
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Schema / Metadata API provides the following features:

1. Execute SQL on the underlying Postgres database, supports schema modifying actions.
2. Modify Hasura metadata (permissions rules and relationships).

This is primarily intended to be used as an ``admin`` API to manage Hasura schema and metadata.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/query`` endpoint.

Request structure
-----------------

.. code-block:: http

   POST /v1/query HTTP/1.1

   {
      "type": "<query-type>",
      "args": <args-object>
   }

Request body
^^^^^^^^^^^^

.. parsed-literal::

   Query_

.. _Query:

Query
*****

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - type
     - true
     - String
     - Type of the query
   * - args
     - true
     - JSON Value
     - The arguments to the query

The various types of queries are listed in the following table:

.. list-table::
   :header-rows: 1

   * - ``type``
     - ``args``
     - Synopsis

   * - **bulk**
     - :ref:`Query <Query>` array
     - Execute multiple operations in a single query

   * - :ref:`run_sql`
     - :ref:`run_sql_args <run_sql_syntax>`
     - Run SQL directly on Postgres

   * - :ref:`track_table`
     - :ref:`TableName <TableName>`
     - Add a table/view

   * - :ref:`untrack_table`
     - :ref:`untrack_table_args <untrack_table_syntax>`
     - Remove a table/view

   * - :ref:`track_function`
     - :ref:`FunctionName <FunctionName>`
     - Add a SQL function

   * - :ref:`untrack_function`
     - :ref:`FunctionName <FunctionName>`
     - Remove a SQL function

   * - :ref:`create_object_relationship`
     - :ref:`create_object_relationship_args <create_object_relationship_syntax>`
     - Define a new object relationship

   * - :ref:`create_array_relationship`
     - :ref:`create_array_relationship_args <create_array_relationship_syntax>`
     - Define a new array relationship

   * - :ref:`drop_relationship`
     - :ref:`drop_relationship_args <drop_relationship_syntax>`
     - Drop an existing relationship

   * - :ref:`set_relationship_comment`
     - :ref:`set_relationship_comment_args <set_relationship_comment_syntax>`
     - Set comment on an existing relationship

   * - :ref:`create_insert_permission`
     - :ref:`create_insert_permission_args <create_insert_permission_syntax>`
     - Specify insert permission

   * - :ref:`drop_insert_permission`
     - :ref:`drop_insert_permission_args <drop_insert_permission_syntax>`
     - Remove existing insert permission

   * - :ref:`create_select_permission`
     - :ref:`create_select_permission_args <create_select_permission_syntax>`
     - Specify select permission

   * - :ref:`drop_select_permission`
     - :ref:`drop_select_permission_args <drop_select_permission_syntax>`
     - Remove existing select permission

   * - :ref:`create_update_permission`
     - :ref:`create_update_permission_args <create_update_permission_syntax>`
     - Specify update permission

   * - :ref:`drop_update_permission`
     - :ref:`drop_update_permission_args <drop_update_permission_syntax>`
     - Remove existing update permission

   * - :ref:`create_delete_permission`
     - :ref:`create_delete_permission_args <create_delete_permission_syntax>`
     - Specify delete permission

   * - :ref:`drop_delete_permission`
     - :ref:`drop_delete_permission_args <drop_delete_permission_syntax>`
     - Remove existing delete permission

   * - :ref:`set_permission_comment`
     - :ref:`set_permission_comment_args <set_permission_comment_syntax>`
     - Set comment on an existing permission

   * - :ref:`create_event_trigger`
     - :ref:`create_event_trigger_args <create_event_trigger_syntax>`
     - Create or replace event trigger

   * - :ref:`delete_event_trigger`
     - :ref:`delete_event_trigger_args <delete_event_trigger_syntax>`
     - Delete existing event trigger

   * - :ref:`export_metadata`
     - ``{}``
     - Export the current metadata

   * - :ref:`replace_metadata`
     - :ref:`replace_metadata_args <replace_metadata_syntax>`
     - Import and replace existing metadata

   * - :ref:`reload_metadata`
     - ``{}``
     - Reload changes to the underlying Postgres DB

   * - :ref:`clear_metadata`
     - ``{}``
     - Clear/wipe-out the current metadata state form server

**See:**

- :doc:`Run SQL <run-sql>`
- :doc:`Tables/Views <table-view>`
- :doc:`Custom SQL Functions <custom-functions>`
- :doc:`Relationships <relationship>`
- :doc:`Permissions <permission>`
- :doc:`Event Triggers <event-triggers>`
- :doc:`Manage Metadata <manage-metadata>`

Response structure
------------------

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Description
     - Response structure

   * - ``200``
     - Success
     - .. parsed-literal::

        Request specific

   * - ``400``
     - Bad request
     - .. code-block:: haskell

          {
              "path"  : String,
              "error" : String
          }

   * - ``401``
     - Unauthorized
     - .. code-block:: haskell

          {
              "error" : String
          }

   * - ``500``
     - Internal server error
     - .. code-block:: haskell

          {
              "error" : String
          }


Error codes
-----------

.. csv-table::
   :file: dataerrors.csv
   :widths: 10, 20, 70
   :header-rows: 1

.. toctree::
  :maxdepth: 1
  :hidden:

  Run SQL <run-sql>
  Tables/Views <table-view>
  Custom Functions <custom-functions>
  Relationships <relationship>
  Permissions <permission>
  Event Triggers <event-triggers>
  Manage Metadata <manage-metadata>
  Syntax definitions <syntax-defs>

