.. meta::
   :description: Hasura metadata API reference
   :keywords: hasura, docs, metadata API, API reference

.. _metadata_apis:

Metadata API Reference (v2.0 and above)
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The metadata API is primarily intended to be used as an ``admin`` API to manage the Hasura metadata.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/metadata`` endpoint.

Request structure
-----------------

.. code-block:: http

   POST /v1/metadata HTTP/1.1

   {
      "type": <query-type>,
      "version": <Integer> (optional),
      "resource_version": <Integer> (optional),
      "args": <args-object>
   }

The structure of args depends on the type and version.

Request body
^^^^^^^^^^^^

.. parsed-literal::

   :ref:`Query <metadata_query>`

.. _metadata_query:

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
   * - resource_version
     - false
     - Integer
     - Version of the resource that you are targeting for replacement
   * - args
     - true
     - JSON Value
     - The arguments to the query
   * - version
     - false
     - Integer
     - Version of the API (inferred by args structure)

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
     - :ref:`Query <metadata_query>` array
     - 1
     - Execute multiple operations in a single query

   * - :ref:`pg_add_source <pg_add_source>`
     - :ref:`pg_add_source_args <pg_add_source_syntax>`
     - 1
     - Add a Postgres database

   * - :ref:`pg_drop_source <pg_drop_source>`
     - :ref:`pg_drop_source_args <pg_drop_source_syntax>`
     - 1
     - Remove a Postgres database

   * - :ref:`pg_track_table <pg_track_table>`
     - :ref:`pg_track_table_args <pg_track_table_syntax>`
     - 1
     - Add a Postgres table/view with configuration

   * - :ref:`pg_untrack_table`
     - :ref:`pg_untrack_table_args <pg_untrack_table_syntax>`
     - 1
     - Remove a Postgres table/view

   * - :ref:`pg_set_table_customization <pg_set_table_customization>`
     - :ref:`pg_set_table_customization_args <pg_set_table_customization_syntax>`
     - 1
     - Set table customization of an already tracked Postgres table

   * - :ref:`pg_set_table_is_enum`
     - :ref:`pg_set_table_is_enum_args <pg_set_table_is_enum_syntax>`
     - 1
     - Set a tracked Postgres table as an enum table

   * - :ref:`pg_track_function`
     - :ref:`pg_track_function_args <pg_track_function_syntax>`
     - 1
     - Add a Postgres SQL function with configuration

   * - :ref:`pg_untrack_function`
     - :ref:`FunctionName <FunctionName>`
     - 1
     - Remove a Postgres SQL function

   * - :ref:`pg_create_function_permission`
     - :ref:`pg_create_function_permission_args <pg_create_function_permission_syntax>`
     - 1
     - Create a Postgres function permission

   * - :ref:`pg_drop_function_permission`
     - :ref:`pg_drop_function_permission_args <pg_drop_function_permission_syntax>`
     - 1
     - Drop an existing Postgres function permission

   * - :ref:`pg_create_object_relationship`
     - :ref:`pg_create_object_relationship_args <pg_create_object_relationship_syntax>`
     - 1
     - Define a new object relationship between Postgres tables/views

   * - :ref:`pg_create_array_relationship`
     - :ref:`pg_create_array_relationship_args <pg_create_array_relationship_syntax>`
     - 1
     - Define a new array relationship between Postgres tables/views

   * - :ref:`pg_drop_relationship`
     - :ref:`pg_drop_relationship_args <pg_drop_relationship_syntax>`
     - 1
     - Drop an existing Postgres relationship

   * - :ref:`pg_rename_relationship`
     - :ref:`pg_rename_relationship_args <pg_rename_relationship_syntax>`
     - 1
     - Modify name of an existing Postgres relationship

   * - :ref:`pg_set_relationship_comment`
     - :ref:`pg_set_relationship_comment_args <pg_set_relationship_comment_syntax>`
     - 1
     - Set comment on an existing Postgres relationship

   * - :ref:`pg_add_computed_field`
     - :ref:`pg_add_computed_field_args <pg_add_computed_field_syntax>`
     - 1
     - Add a computed field to a Postgres table/view

   * - :ref:`pg_drop_computed_field`
     - :ref:`pg_drop_computed_field_args <pg_drop_computed_field_syntax>`
     - 1
     - Drop a Postgres computed field

   * - :ref:`pg_create_insert_permission`
     - :ref:`pg_create_insert_permission_args <pg_create_insert_permission_syntax>`
     - 1
     - Specify insert permission for a Postgres table/view

   * - :ref:`pg_drop_insert_permission`
     - :ref:`pg_drop_insert_permission_args <pg_drop_insert_permission_syntax>`
     - 1
     - Remove existing insert permission for a Postgres table/view

   * - :ref:`pg_create_select_permission`
     - :ref:`pg_create_select_permission_args <pg_create_select_permission_syntax>`
     - 1
     - Specify select permission for a Postgres table/view

   * - :ref:`pg_drop_select_permission`
     - :ref:`pg_drop_select_permission_args <pg_drop_select_permission_syntax>`
     - 1
     - Remove existing select permission for a Postgres table/view

   * - :ref:`pg_create_update_permission`
     - :ref:`pg_create_update_permission_args <pg_create_update_permission_syntax>`
     - 1
     - Specify update permission for a Postgres table/view

   * - :ref:`pg_drop_update_permission`
     - :ref:`pg_drop_update_permission_args <pg_drop_update_permission_syntax>`
     - 1
     - Remove existing update permission for a Postgres table/view

   * - :ref:`pg_create_delete_permission`
     - :ref:`pg_create_delete_permission_args <pg_create_delete_permission_syntax>`
     - 1
     - Specify delete permission for a Postgres table/view

   * - :ref:`pg_drop_delete_permission`
     - :ref:`pg_drop_delete_permission_args <pg_drop_delete_permission_syntax>`
     - 1
     - Remove existing delete permission for a Postgres table/view

   * - :ref:`pg_set_permission_comment`
     - :ref:`pg_set_permission_comment_args <pg_set_permission_comment_syntax>`
     - 1
     - Set comment on an existing permission for a Postgres table/view

   * - :ref:`pg_create_event_trigger`
     - :ref:`pg_create_event_trigger_args <pg_create_event_trigger_syntax>`
     - 1
     - Create or replace an event trigger on a Postgres table

   * - :ref:`pg_delete_event_trigger`
     - :ref:`pg_delete_event_trigger_args <pg_delete_event_trigger_syntax>`
     - 1
     - Delete an existing event trigger on a Postgres table

   * - :ref:`pg_redeliver_event`
     - :ref:`pg_redeliver_event_args <pg_redeliver_event_syntax>`
     - 1
     - Redeliver an existing event on a Postgres table

   * - :ref:`pg_invoke_event_trigger`
     - :ref:`pg_invoke_event_trigger_args <pg_invoke_event_trigger_syntax>`
     - 1
     - Invoke a trigger with custom payload on a Postgres table

   * - :ref:`mssql_add_source <mssql_add_source>`
     - :ref:`mssql_add_source_args <mssql_add_source_syntax>`
     - 1
     - Add an MS SQL Server database

   * - :ref:`mssql_drop_source <mssql_drop_source>`
     - :ref:`mssql_drop_source_args <mssql_drop_source_syntax>`
     - 1
     - Remove an MS SQL Server database

   * - :ref:`mssql_track_table <mssql_track_table>`
     - :ref:`mssql_track_table_args <mssql_track_table_syntax>`
     - 1
     - Add an MS SQL Server table/view with configuration

   * - :ref:`mssql_untrack_table`
     - :ref:`mssql_untrack_table_args <mssql_untrack_table_syntax>`
     - 1
     - Remove an MS SQL Server table/view

   * - :ref:`mssql_create_object_relationship`
     - :ref:`mssql_create_object_relationship_args <mssql_create_object_relationship_syntax>`
     - 1
     - Define a new object relationship between MS SQL Server tables/views

   * - :ref:`mssql_create_array_relationship`
     - :ref:`mssql_create_array_relationship_args <mssql_create_array_relationship_syntax>`
     - 1
     - Define a new array relationship between MS SQL Server tables/views

   * - :ref:`mssql_drop_relationship`
     - :ref:`mssql_drop_relationship_args <mssql_drop_relationship_syntax>`
     - 1
     - Drop an existing MS SQL Server relationship

   * - :ref:`mssql_rename_relationship`
     - :ref:`mssql_rename_relationship_args <mssql_rename_relationship_syntax>`
     - 1
     - Modify name of an existing MS SQL Server relationship

   * - :ref:`mssql_set_relationship_comment`
     - :ref:`mssql_set_relationship_comment_args <mssql_set_relationship_comment_syntax>`
     - 1
     - Set comment on an existing MS SQL Server relationship

   * - :ref:`mssql_set_table_customization <mssql_set_table_customization>`
     - :ref:`mssql_set_table_customization_args <mssql_set_table_customization_syntax>`
     - 1
     - Set table customization of an already tracked MS SQL Server table

   * - :ref:`mssql_create_insert_permission`
     - :ref:`mssql_create_insert_permission_args <mssql_create_insert_permission_syntax>`
     - 1
     - Specify insert permission for an MS SQL Server table/view

   * - :ref:`mssql_drop_insert_permission`
     - :ref:`mssql_drop_insert_permission_args <mssql_drop_insert_permission_syntax>`
     - 1
     - Remove existing insert permission for an MS SQL Server table/view

   * - :ref:`mssql_create_select_permission`
     - :ref:`mssql_create_select_permission_args <mssql_create_select_permission_syntax>`
     - 1
     - Specify select permission for an MS SQL Server table/view

   * - :ref:`mssql_drop_select_permission`
     - :ref:`mssql_drop_select_permission_args <mssql_drop_select_permission_syntax>`
     - 1
     - Remove existing select permission for an MS SQL Server table/view

   * - :ref:`mssql_create_update_permission`
     - :ref:`mssql_create_update_permission_args <mssql_create_update_permission_syntax>`
     - 1
     - Specify update permission for an MS SQL Server table/view

   * - :ref:`mssql_drop_update_permission`
     - :ref:`mssql_drop_update_permission_args <mssql_drop_update_permission_syntax>`
     - 1
     - Remove existing update permission for an MS SQL Server table/view

   * - :ref:`mssql_create_delete_permission`
     - :ref:`mssql_create_delete_permission_args <mssql_create_delete_permission_syntax>`
     - 1
     - Specify delete permission for an MS SQL Server table/view

   * - :ref:`mssql_drop_delete_permission`
     - :ref:`mssql_drop_delete_permission_args <mssql_drop_delete_permission_syntax>`
     - 1
     - Remove existing delete permission for an MS SQL Server table/view

   * - :ref:`mssql_set_permission_comment`
     - :ref:`mssql_set_permission_comment_args <mssql_set_permission_comment_syntax>`
     - 1
     - Set comment on an existing permission for an MS SQL Server table/view

   * - :ref:`metadata_create_cron_trigger`
     - :ref:`create_cron_trigger_args <metadata_create_cron_trigger_syntax>`
     - 1
     - Create a cron trigger

   * - :ref:`metadata_delete_cron_trigger`
     - :ref:`delete_cron_trigger_args <metadata_delete_cron_trigger_syntax>`
     - 1
     - Delete an existing cron trigger

   * - :ref:`metadata_create_scheduled_event`
     - :ref:`create_scheduled_event_args <metadata_create_scheduled_event_syntax>`
     - 1
     - Create a new scheduled event

   * - :ref:`metadata_add_remote_schema`
     - :ref:`add_remote_schema_args <metadata_add_remote_schema_syntax>`
     - 1
     - Add a remote GraphQL server as a remote schema

   * - :ref:`metadata_update_remote_schema`
     - :ref:`update_remote_schema_args <metadata_update_remote_schema_syntax>`
     - 1
     - Update the details for a remote schema

   * - :ref:`metadata_remove_remote_schema`
     - :ref:`remove_remote_schema_args <metadata_remove_remote_schema_syntax>`
     - 1
     - Remove an existing remote schema

   * - :ref:`metadata_reload_remote_schema`
     - :ref:`reload_remote_schema_args <metadata_reload_remote_schema_syntax>`
     - 1
     - Reload schema of an existing remote schema

   * - :ref:`metadata_add_remote_schema_permissions`
     - :ref:`add_remote_schema_permissions <metadata_add_remote_schema_permissions_syntax>`
     - 1
     - Add permissions to a role of an existing remote schema

   * - :ref:`metadata_drop_remote_schema_permissions`
     - :ref:`drop_remote_schema_permissions <metadata_drop_remote_schema_permissions_syntax>`
     - 1
     - Drop existing permissions defined for a role for a remote schema

   * - :ref:`metadata_create_remote_relationship`
     - :ref:`create_remote_relationship_args <metadata_create_remote_relationship_syntax>`
     - 1
     - Create a remote relationship with an existing remote schema

   * - :ref:`metadata_update_remote_relationship`
     - :ref:`update_remote_relationship_args <metadata_update_remote_relationship_syntax>`
     - 1
     - Update an existing remote relationship

   * - :ref:`metadata_delete_remote_relationship`
     - :ref:`delete_remote_relationship_args <metadata_delete_remote_relationship_syntax>`
     - 1
     - Delete an existing remote relationship

   * - :ref:`metadata_export_metadata`
     - :ref:`Empty Object`
     - 1
     - Export the current metadata

   * - :ref:`metadata_export_metadata`
     - :ref:`Empty Object`
     - 2
     - Export existing metadata with resource version included.

   * - :ref:`metadata_replace_metadata`
     - :ref:`replace_metadata_args <metadata_replace_metadata_syntax>`
     - 1
     - Import and replace existing metadata


   * - :ref:`metadata_replace_metadata`
     - :ref:`replace_metadata_args <metadata_replace_metadata_syntax>`
     - 2
     - Replace existing metadata with check against current resource_version.

   * - :ref:`metadata_reload_metadata`
     - :ref:`reload_metadata_args <metadata_reload_metadata_syntax>`
     - 1
     - Reload changes to the underlying Postgres DB

   * - :ref:`metadata_clear_metadata`
     - :ref:`Empty Object`
     - 1
     - Clear/wipe-out the current metadata state form server

   * - :ref:`metadata_get_inconsistent_metadata`
     - :ref:`Empty Object`
     - 1
     - List all inconsistent metadata objects

   * - :ref:`metadata_drop_inconsistent_metadata`
     - :ref:`Empty Object`
     - 1
     - Drop all inconsistent metadata objects

   * - :ref:`metadata_create_query_collection`
     - :ref:`create_query_collection_args <metadata_create_query_collection_syntax>`
     - 1
     - Create a query collection

   * - :ref:`metadata_drop_query_collection`
     - :ref:`drop_query_collection_args <metadata_drop_query_collection_syntax>`
     - 1
     - Drop a query collection

   * - :ref:`metadata_add_query_to_collection`
     - :ref:`add_query_to_collection_args <metadata_add_query_to_collection_syntax>`
     - 1
     - Add a query to a given collection

   * - :ref:`metadata_drop_query_from_collection`
     - :ref:`drop_query_from_collection_args <metadata_drop_query_from_collection_syntax>`
     - 1
     - Drop a query from a given collection

   * - :ref:`metadata_add_collection_to_allowlist`
     - :ref:`add_collection_to_allowlist_args <metadata_add_collection_to_allowlist_syntax>`
     - 1
     - Add a collection to the allow-list

   * - :ref:`metadata_drop_collection_from_allowlist`
     - :ref:`drop_collection_from_allowlist_args <metadata_drop_collection_from_allowlist_syntax>`
     - 1
     - Drop a collection from the allow-list

   * - :ref:`metadata_set_custom_types`
     - :ref:`set_custom_types_args <metadata_set_custom_types_syntax>`
     - 1
     - Set custom GraphQL types

   * - :ref:`metadata_create_action`
     - :ref:`create_action_args <metadata_create_action_syntax>`
     - 1
     - Create an action

   * - :ref:`metadata_drop_action`
     - :ref:`drop_action_args <metadata_drop_action_syntax>`
     - 1
     - Drop an action

   * - :ref:`metadata_update_action`
     - :ref:`update_action_args <metadata_update_action_syntax>`
     - 1
     - Update an action

   * - :ref:`metadata_create_action_permission`
     - :ref:`create_action_permission_args <metadata_create_action_permission_syntax>`
     - 1
     - Create an action permission

   * - :ref:`metadata_drop_action_permission`
     - :ref:`drop_action_permission_args <metadata_drop_action_permission_syntax>`
     - 1
     - Drop an action permission

   * - :ref:`metadata_create_rest_endpoint`
     - :ref:`create_rest_endpoint_args <metadata_create_rest_endpoint_syntax>`
     - 1
     - Create a RESTified GraphQL Endpoint

   * - :ref:`metadata_drop_rest_endpoint`
     - :ref:`drop_rest_endpoint_args <metadata_drop_rest_endpoint_syntax>`
     - 1
     - Drop a RESTified GraphQL Endpoint

   * - :ref:`metadata_add_inherited_role`
     - :ref:`add_inherited_role_args <metadata_add_inherited_role_syntax>`
     - 1
     - Add an inherited role

   * - :ref:`metadata_drop_inherited_role`
     - :ref:`drop_inherited_role_args <metadata_drop_inherited_role_syntax>`
     - 1
     - Drop an inherited role

   * - :ref:`metadata_set_graphql_introspection_options`
     - :ref:`set_graphql_schema_introspection_options_args <set_graphql_schema_introspection_options_syntax>`
     - 1
     - Set graphql introspection options


.. TODO: MSSQL_UNSUPPORTED

  * - :ref:`mssql_set_table_is_enum`
  - :ref:`mssql_set_table_is_enum_args <mssql_set_table_is_enum_syntax>`
  - 1
  - Set a tracked MS SQL Server table as an enum table

  * - :ref:`mssql_track_function`
  - :ref:`mssql_track_function_args <mssql_track_function_syntax>`
  - 1
  - Add an MS SQL Server SQL function with configuration

  * - :ref:`mssql_untrack_function`
  - :ref:`FunctionName <FunctionName>`
  - 1
  - Remove an MS SQL Server SQL function

  * - :ref:`mssql_create_function_permission`
  - :ref:`mssql_create_function_permission_args <mssql_create_function_permission_syntax>`
  - 1
  - Create an MS SQL Server function permission

  * - :ref:`mssql_drop_function_permission`
  - :ref:`mssql_drop_function_permission_args <mssql_drop_function_permission_syntax>`
  - 1
  - Drop an existing MS SQL Server function permission

  * - :ref:`mssql_add_computed_field`
  - :ref:`mssql_add_computed_field_args <mssql_add_computed_field_syntax>`
  - 1
  - Add a computed field to an MS SQL Server table/view

  * - :ref:`mssql_drop_computed_field`
  - :ref:`mssql_drop_computed_field_args <mssql_drop_computed_field_syntax>`
  - 1
  - Drop an MS SQL Server computed field


  * - :ref:`mssql_create_event_trigger`
  - :ref:`mssql_create_event_trigger_args <mssql_create_event_trigger_syntax>`
  - 1
  - Create or replace an event trigger on an MS SQL Server table

  * - :ref:`mssql_delete_event_trigger`
  - :ref:`mssql_delete_event_trigger_args <mssql_delete_event_trigger_syntax>`
  - 1
  - Delete an existing event trigger on an MS SQL Server table

  * - :ref:`mssql_redeliver_event`
  - :ref:`mssql_redeliver_event_args <mssql_redeliver_event_syntax>`
  - 1
  - Redeliver an existing event on an MS SQL Server table

  * - :ref:`mssql_invoke_event_trigger`
  - :ref:`mssql_invoke_event_trigger_args <mssql_invoke_event_trigger_syntax>`
  - 1
  - Invoke a trigger with custom payload on an MS SQL Server table


**See:**

- :ref:`Tables/Views <metadata_api_tables_views>`
- :ref:`Custom SQL Functions <metadata_api_custom_functions>`
- :ref:`Relationships <metadata_api_relationship>`
- :ref:`Computed Fields <metadata_api_computed_field>`
- :ref:`Permissions <metadata_api_permission>`
- :ref:`Inherited Roles <metadata_inherited_role>`
- :ref:`Remote Schema Permissions <metadata_remote_schema_api_permission>`
- :ref:`Event Triggers <metadata_api_event_triggers>`
- :ref:`Remote Schemas <metadata_api_remote_schemas>`
- :ref:`Query Collections <metadata_api_query_collections>`
- :ref:`Custom Types <metadata_api_custom_types>`
- :ref:`Actions <metadata_api_actions>`
- :ref:`Manage Metadata <metadata_api_manage_metadata>`


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

Disabling metadata API
----------------------

Since this API can be used to make changes to the GraphQL schema, it can be
disabled, especially in production deployments.

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be used to
enable/disable this API. By default, the schema/metadata API is enabled. To disable it, you need
to explicitly state that this API is not enabled i.e. remove it from the list of enabled APIs.

.. code-block:: bash

   # enable only graphql api, disable metadata and pgdump
   --enabled-apis="graphql"
   HASURA_GRAPHQL_ENABLED_APIS="graphql"

See :ref:`server_flag_reference` for info on setting the above flag/env var.


.. _metadata_resource_version:

Metadata Resource Versioning
----------------------------

Metadata is versioned with an optional ``resource_version`` field in operations and responses.

This is intended to allow for feedback when replacing metadata with modifications to an out-of-date copy.

The ``resource_version`` supplied must match the version returned otherwise a 409 error response is returned.

The version is incremented on any operation that modified metadata as well as ``reload_metadata``.

.. toctree::
  :maxdepth: 1
  :hidden:

  Databases <source>
  Tables/Views <table-view>
  Custom Functions <custom-functions>
  Relationships <relationship>
  Permissions <permission>
  Inherited Roles <inherited-roles>
  Remote Schema Permissions <remote-schema-permissions>
  Computed Fields <computed-field>
  Event Triggers <event-triggers>
  Scheduled Triggers <scheduled-triggers>
  Remote Schemas <remote-schemas>
  Remote Relationships <remote-relationships>
  Query Collections <query-collections>
  RESTified GraphQL Endpoints <restified-endpoints>
  Custom Types <custom-types>
  Actions <actions>
  Manage Metadata <manage-metadata>
  Introspection options <introspection>
