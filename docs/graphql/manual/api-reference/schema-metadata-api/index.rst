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

Request types
-------------

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

   * - :ref:`invoke_event_trigger`
     - :ref:`invoke_event_trigger_args <invoke_event_trigger_syntax>`
     - Invoke trigger manually

   * - :ref:`delete_event_trigger`
     - :ref:`delete_event_trigger_args <delete_event_trigger_syntax>`
     - Delete existing event trigger

   * - :ref:`add_remote_schema`
     - :ref:`add_remote_schema_args <add_remote_schema_syntax>`
     - Add a remote GraphQL server as remote schema

   * - :ref:`remove_remote_schema`
     - :ref:`remove_remote_schema_args <remove_remote_schema_syntax>`
     - Remove existing remote schema

   * - :ref:`reload_remote_schema`
     - :ref:`reload_remote_schema_args <reload_remote_schema_syntax>`
     - Reload schema of existing remote server

   * - :ref:`export_metadata`
     - :ref:`Empty Object`
     - Export the current metadata

   * - :ref:`replace_metadata`
     - :ref:`replace_metadata_args <replace_metadata_syntax>`
     - Import and replace existing metadata

   * - :ref:`reload_metadata`
     - :ref:`Empty Object`
     - Reload changes to the underlying Postgres DB

   * - :ref:`clear_metadata`
     - :ref:`Empty Object`
     - Clear/wipe-out the current metadata state form server

   * - :ref:`get_inconsistent_metadata`
     - :ref:`Empty Object`
     - List all inconsistent metadata objects

   * - :ref:`drop_inconsistent_metadata`
     - :ref:`Empty Object`
     - Drop all inconsistent metadata objects

   * - :ref:`create_query_collection`
     - :ref:`create_query_collection_args <create_query_collection_syntax>`
     - Create a query collection

   * - :ref:`drop_query_collection`
     - :ref:`drop_query_collection_args <drop_query_collection_syntax>`
     - Drop a query collection

   * - :ref:`add_query_to_collection`
     - :ref:`add_query_to_collection_args <add_query_to_collection_syntax>`
     - Add a query to given collection

   * - :ref:`drop_query_from_collection`
     - :ref:`drop_query_from_collection_args <drop_query_from_collection_syntax>`
     - Drop a query from given collection

   * - :ref:`add_collection_to_allowlist`
     - :ref:`add_collection_to_allowlist_args <add_collection_to_allowlist_syntax>`
     - Add a collection to allow-list

   * - :ref:`drop_collection_from_allowlist`
     - :ref:`drop_collection_from_allowlist_args <drop_collection_from_allowlist_syntax>`
     - Drop a collection from allow-list

**See:**

- :doc:`Run SQL <run-sql>`
- :doc:`Tables/Views <table-view>`
- :doc:`Custom SQL Functions <custom-functions>`
- :doc:`Relationships <relationship>`
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

Disabling Schema/Metadata API
-----------------------------

Since this API can be used to make changes to the GraphQL schema, it can be
disabled, especially in production deployments.

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be used to
enable/disable this API. By default, The schema/metadata API is enabled. To disable it, you need
to explicitly state that this API is not enabled. i.e. remove it from the list of enabled APIs.

.. code-block:: bash

   # enable only graphql api, disable metadata and pgdump
   --enabled-apis="graphql"
   HASURA_GRAPHQL_ENABLED_APIS="graphql"

See :doc:`../../deployment/graphql-engine-flags/reference` for info on setting the above flag/env var

.. toctree::
  :maxdepth: 1
  :hidden:

  Run SQL <run-sql>
  Tables/Views <table-view>
  Custom Functions <custom-functions>
  Relationships <relationship>
  Permissions <permission>
  Event Triggers <event-triggers>
  Remote Schemas <remote-schemas>
  Query Collections <query-collections>
  Manage Metadata <manage-metadata>
  Syntax definitions <syntax-defs>
