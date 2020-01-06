.. _metadata_apis:

Schema / Metadata API Reference
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The schema / metadata API provides the following features:

1. Execute SQL on the underlying Postgres database, supports schema modifying actions.
2. Modify Hasura metadata (permission rules and relationships).

This is primarily intended to be used as an ``admin`` API to manage the Hasura schema and metadata.

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
   * - version
     - false
     - Integer
     - Version of the API (default: 1)

Request types
-------------

The various types of queries are listed in the following table:

.. list-table::
   :header-rows: 1

   * - ``type``
     - ``args``
     - ``version``
     - Synopsis

   * - **bulk**
     - :ref:`Query <Query>` array
     - 1
     - Execute multiple operations in a single query

   * - :ref:`run_sql`
     - :ref:`run_sql_args <run_sql_syntax>`
     - 1
     - Run SQL directly on Postgres

   * - :ref:`track_table`
     - :ref:`TableName <TableName>`
     - 1
     - Add a table/view

   * - :ref:`track_table <track_table_v2>`
     - :ref:`track_table_args <track_table_args_syntax_v2>`
     - 2
     - Add a table/view with configuration

   * - :ref:`set_table_custom_fields <set_table_custom_fields>`
     - :ref:`set_table_custom_fields_args <set_table_custom_fields_args_syntax>`
     - 2
     - Set custom fields to an already tracked table

   * - :ref:`untrack_table`
     - :ref:`untrack_table_args <untrack_table_syntax>`
     - 1
     - Remove a table/view

   * - :ref:`track_function`
     - :ref:`FunctionName <FunctionName>`
     - 1
     - Add an SQL function

   * - :ref:`track_function`
     - :ref:`track_function_args <track_function_args_syntax_v2>`
     - 2
     - Add an SQL function with configuration

   * - :ref:`untrack_function`
     - :ref:`FunctionName <FunctionName>`
     - 1
     - Remove an SQL function

   * - :ref:`create_object_relationship`
     - :ref:`create_object_relationship_args <create_object_relationship_syntax>`
     - 1
     - Define a new object relationship

   * - :ref:`create_array_relationship`
     - :ref:`create_array_relationship_args <create_array_relationship_syntax>`
     - 1
     - Define a new array relationship

   * - :ref:`drop_relationship`
     - :ref:`drop_relationship_args <drop_relationship_syntax>`
     - 1
     - Drop an existing relationship

   * - :ref:`rename_relationship`
     - :ref:`rename_relationship_args <rename_relationship_syntax>`
     - 1
     - Modify name of an existing relationship

   * - :ref:`set_relationship_comment`
     - :ref:`set_relationship_comment_args <set_relationship_comment_syntax>`
     - 1
     - Set comment on an existing relationship

   * - :ref:`add_computed_field`
     - :ref:`add_computed_field_args <add_computed_field_syntax>`
     - 1
     - Add a computed field

   * - :ref:`drop_computed_field`
     - :ref:`drop_computed_field_args <drop_computed_field_syntax>`
     - 1
     - Drop a computed field

   * - :ref:`create_insert_permission`
     - :ref:`create_insert_permission_args <create_insert_permission_syntax>`
     - 1
     - Specify insert permission

   * - :ref:`drop_insert_permission`
     - :ref:`drop_insert_permission_args <drop_insert_permission_syntax>`
     - 1
     - Remove existing insert permission

   * - :ref:`create_select_permission`
     - :ref:`create_select_permission_args <create_select_permission_syntax>`
     - 1
     - Specify select permission

   * - :ref:`drop_select_permission`
     - :ref:`drop_select_permission_args <drop_select_permission_syntax>`
     - 1
     - Remove existing select permission

   * - :ref:`create_update_permission`
     - :ref:`create_update_permission_args <create_update_permission_syntax>`
     - 1
     - Specify update permission

   * - :ref:`drop_update_permission`
     - :ref:`drop_update_permission_args <drop_update_permission_syntax>`
     - 1
     - Remove existing update permission

   * - :ref:`create_delete_permission`
     - :ref:`create_delete_permission_args <create_delete_permission_syntax>`
     - 1
     - Specify delete permission

   * - :ref:`drop_delete_permission`
     - :ref:`drop_delete_permission_args <drop_delete_permission_syntax>`
     - 1
     - Remove existing delete permission

   * - :ref:`set_permission_comment`
     - :ref:`set_permission_comment_args <set_permission_comment_syntax>`
     - 1
     - Set comment on an existing permission

   * - :ref:`create_event_trigger`
     - :ref:`create_event_trigger_args <create_event_trigger_syntax>`
     - 1
     - Create or replace an event trigger

   * - :ref:`invoke_event_trigger`
     - :ref:`invoke_event_trigger_args <invoke_event_trigger_syntax>`
     - 1
     - Invoke a trigger manually

   * - :ref:`delete_event_trigger`
     - :ref:`delete_event_trigger_args <delete_event_trigger_syntax>`
     - 1
     - Delete an existing event trigger

   * - :ref:`add_remote_schema`
     - :ref:`add_remote_schema_args <add_remote_schema_syntax>`
     - 1
     - Add a remote GraphQL server as a remote schema

   * - :ref:`remove_remote_schema`
     - :ref:`remove_remote_schema_args <remove_remote_schema_syntax>`
     - 1
     - Remove an existing remote schema

   * - :ref:`reload_remote_schema`
     - :ref:`reload_remote_schema_args <reload_remote_schema_syntax>`
     - 1
     - Reload schema of an existing remote schema

   * - :ref:`export_metadata`
     - :ref:`Empty Object`
     - 1
     - Export the current metadata

   * - :ref:`replace_metadata`
     - :ref:`replace_metadata_args <replace_metadata_syntax>`
     - 1
     - Import and replace existing metadata

   * - :ref:`reload_metadata`
     - :ref:`Empty Object`
     - 1
     - Reload changes to the underlying Postgres DB

   * - :ref:`clear_metadata`
     - :ref:`Empty Object`
     - 1
     - Clear/wipe-out the current metadata state form server

   * - :ref:`get_inconsistent_metadata`
     - :ref:`Empty Object`
     - 1
     - List all inconsistent metadata objects

   * - :ref:`drop_inconsistent_metadata`
     - :ref:`Empty Object`
     - 1
     - Drop all inconsistent metadata objects

   * - :ref:`create_query_collection`
     - :ref:`create_query_collection_args <create_query_collection_syntax>`
     - 1
     - Create a query collection

   * - :ref:`drop_query_collection`
     - :ref:`drop_query_collection_args <drop_query_collection_syntax>`
     - 1
     - Drop a query collection

   * - :ref:`add_query_to_collection`
     - :ref:`add_query_to_collection_args <add_query_to_collection_syntax>`
     - 1
     - Add a query to a given collection

   * - :ref:`drop_query_from_collection`
     - :ref:`drop_query_from_collection_args <drop_query_from_collection_syntax>`
     - 1
     - Drop a query from a given collection

   * - :ref:`add_collection_to_allowlist`
     - :ref:`add_collection_to_allowlist_args <add_collection_to_allowlist_syntax>`
     - 1
     - Add a collection to the allow-list

   * - :ref:`drop_collection_from_allowlist`
     - :ref:`drop_collection_from_allowlist_args <drop_collection_from_allowlist_syntax>`
     - 1
     - Drop a collection from the allow-list

**See:**

- :doc:`Run SQL <run-sql>`
- :doc:`Tables/Views <table-view>`
- :doc:`Custom SQL Functions <custom-functions>`
- :doc:`Relationships <relationship>`
- :doc:`Computed Fields <computed-field>`
- :doc:`Permissions <permission>`
- :doc:`Event Triggers <event-triggers>`
- :doc:`Remote Schemas <remote-schemas>`
- :doc:`Query Collections <query-collections>`
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

Disabling schema / metadata API
-------------------------------

Since this API can be used to make changes to the GraphQL schema, it can be
disabled, especially in production deployments.

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be used to
enable/disable this API. By default, the schema/metadata API is enabled. To disable it, you need
to explicitly state that this API is not enabled i.e. remove it from the list of enabled APIs.

.. code-block:: bash

   # enable only graphql api, disable metadata and pgdump
   --enabled-apis="graphql"
   HASURA_GRAPHQL_ENABLED_APIS="graphql"

See :doc:`../../deployment/graphql-engine-flags/reference` for info on setting the above flag/env var.

.. toctree::
  :maxdepth: 1
  :hidden:

  Run SQL <run-sql>
  Tables/Views <table-view>
  Custom Functions <custom-functions>
  Relationships <relationship>
  Permissions <permission>
  Computed Fields <computed-field>
  Event Triggers <event-triggers>
  Remote Schemas <remote-schemas>
  Query Collections <query-collections>
  Manage Metadata <manage-metadata>
  Common syntax definitions <syntax-defs>
