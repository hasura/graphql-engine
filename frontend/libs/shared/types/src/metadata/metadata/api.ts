import { OpenTelemetryQueries } from '../openTelemetry';
import { KnownEnterpriseDriver } from '../source';

export type LatestReleaseVersionResult = {
  latest: string;
  prerelease: string;
};

export const METADATA_QUERY_TYPES = [
  'create_remote_relationship',
  'create_insert_permission',
  'drop_insert_permission',
  'create_select_permission',
  'drop_select_permission',
  'create_update_permission',
  'drop_update_permission',
  'create_delete_permission',
  'drop_delete_permission',
  'set_permission_comment',
  'set_function_customization',
  'track_table',
  'track_tables',
  'untrack_table',
  'untrack_tables',
  'set_table_is_enum',
  'set_table_customization',
  'set_apollo_federation_config',
  'add_source',
  'update_source',
  'drop_source',
  'get_source_tables',
  'track_function',
  'untrack_function',
  'create_function_permission',
  'drop_function_permission',
  'track_logical_model',
  'untrack_logical_model',
  'create_logical_model_select_permission',
  'drop_logical_model_select_permission',
  'create_event_trigger',
  'delete_event_trigger',
  'redeliver_event',
  'invoke_event_trigger',
  'get_event_logs',
  'get_event_invocation_logs',
  'get_event_by_id',
  'track_native_query',
  'untrack_native_query',
  'create_remote_relationship',
  'update_remote_relationship',
  'delete_remote_relationship',
  'create_object_relationship',
  'create_array_relationship',
  'drop_relationship',
  'set_relationship_comment',
  'rename_relationship',
  'suggest_relationships',
] as const;

export const ALLOWED_METADATA_TYPES = [
  'create_remote_schema_remote_relationship',
  'update_remote_schema_remote_relationship',
  'delete_remote_schema_remote_relationship',
  'add_remote_schema',
  'update_scope_of_collection_in_allowlist',
  'drop_collection_from_allowlist',
  'set_api_limits',
  'remove_api_limits',
  'set_graphql_schema_introspection_options',
  // NOTE: permission operations (create/drop_{insert,select,update,delete}_permission,
  // set_permission_comment) are NOT listed here: the HGE `/v1/metadata` parser only
  // accepts them backend-prefixed (e.g. `pg_create_select_permission`). The prefixed
  // forms are produced via `METADATA_QUERY_TYPES` -> `AllMetadataQueries` above.
  'get_inconsistent_metadata',
  'drop_inconsistent_metadata',
  'add_remote_schema',
  'update_remote_schema',
  'remove_remote_schema',
  'reload_remote_schema',
  'introspect_remote_schema',
  'create_cron_trigger',
  'delete_cron_trigger',
  'create_scheduled_event',
  'create_query_collection',
  'drop_query_collection',
  'rename_query_collection',
  'add_query_to_collection',
  'drop_query_from_collection',
  'add_collection_to_allowlist',
  'drop_collection_from_allowlist',
  'replace_metadata',
  'export_metadata',
  'clear_metadata',
  'reload_metadata',
  'create_action',
  'drop_action',
  'update_action',
  'create_action_permission',
  'drop_action_permission',
  'set_custom_types',
  'dump_internal_state',
  'get_catalog_state',
  'set_catalog_state',
  'get_scheduled_event_invocations',
  'get_scheduled_events',
  'delete_scheduled_event',
  'create_rest_endpoint',
  'drop_rest_endpoint',
  'add_host_to_tls_allowlist',
  'drop_host_from_tls_allowlist',
  'dc_add_agent',
  'dc_delete_agent',
  'pg_test_connection_template',
  'drop_remote_schema_permissions',
  'rename_source',
  'pg_add_computed_field',
  'pg_drop_computed_field',
  'bigquery_add_computed_field',
  'bigquery_drop_computed_field',
  'mssql_track_stored_procedure',
  'mssql_untrack_stored_procedure',
  'cleanup_event_trigger_logs',
  'resume_event_trigger_cleanups',
  'pause_event_trigger_cleanups',
  'add_inherited_role',
  'drop_inherited_role',
  // GDC query types
  'get_source_kind_capabilities',
  'get_table_info',
  'reference_get_source_trackables',
  'dataconnector_get_source_tables',
  'list_source_kinds',
  'get_cron_triggers',
] as const;

// Bulk wrappers supported by the HGE `/v1/metadata` endpoint.
// NOTE: `concurrent_bulk` is intentionally NOT here — the metadata endpoint does
// not parse it. It is only supported by the `/v2/query` endpoint (see
// `BULK_QUERY_TYPES` below).
export const BULK_METADATA_TYPES = [
  'bulk',
  'bulk_atomic',
  'bulk_keep_going',
] as const;

export type BulkMetadataQueryType = (typeof BULK_METADATA_TYPES)[number];

// Bulk wrappers supported by the HGE `/v2/query` endpoint (run_sql etc.).
// The metadata-only wrappers (`bulk_atomic`, `bulk_keep_going`) are not valid here.
export const BULK_QUERY_TYPES = ['bulk', 'concurrent_bulk'] as const;

export type BulkQueryType = (typeof BULK_QUERY_TYPES)[number];

type SupportedMetadataDataSourcesPrefix =
  'mssql' | 'bigquery' | 'pg' | KnownEnterpriseDriver;

type MetadataQueryType = (typeof METADATA_QUERY_TYPES)[number];

export type AllMetadataQueries =
  `${SupportedMetadataDataSourcesPrefix}_${MetadataQueryType}`;

type SupportedRunSQLDataSourcesPrefix =
  'mssql' | 'bigquery' | 'pg' | 'citus' | 'cockroach';

export type AllowedRunSQLKeys = `${SupportedRunSQLDataSourcesPrefix}_run_sql`;

export type SingleMetadataTypes =
  | (typeof ALLOWED_METADATA_TYPES)[number]
  | AllMetadataQueries
  | OpenTelemetryQueries;

export type AllowedMetadataTypes = SingleMetadataTypes | BulkMetadataQueryType;
