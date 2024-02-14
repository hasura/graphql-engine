import type { HasuraMetadataV3 } from '../../metadata/types';
import type { MetadataQueryType } from '../../metadata/queryUtils';
import type { OpenTelemetryQueries } from '../hasura-metadata-types';
import type { IntrospectionQuery } from 'graphql';
import type { RemoteField } from '../RemoteRelationships/RemoteSchemaRelationships/types';

import type { DataTarget } from '../Datasources';

export interface MetadataResponse {
  resource_version: number;
  metadata: HasuraMetadataV3;
}

export interface SchemaResponse {
  data: IntrospectionQuery;
}

export const allowedMetadataTypesArr = [
  'pg_create_remote_relationship',
  'pg_create_insert_permission',
  'pg_drop_insert_permission',
  'pg_create_select_permission',
  'pg_drop_select_permission',
  'pg_create_update_permission',
  'pg_drop_update_permission',
  'pg_create_delete_permission',
  'pg_drop_delete_permission',
  'pg_set_permission_comment',
  'pg_track_table',
  'pg_set_function_customization',
  'mssql_create_insert_permission',
  'mssql_drop_insert_permission',
  'mssql_create_select_permission',
  'mssql_drop_select_permission',
  'mssql_create_update_permission',
  'mssql_drop_update_permission',
  'mssql_create_delete_permission',
  'mssql_drop_delete_permission',
  'mssql_set_permission_comment',
  'create_remote_schema_remote_relationship',
  'update_remote_schema_remote_relationship',
  'delete_remote_schema_remote_relationship',
  'add_remote_schema',
  'update_scope_of_collection_in_allowlist',
  'drop_collection_from_allowlist',
  'add_collection_from_allowlist',
  'bulk',
] as const;

export interface DbToRemoteSchemaRelationship {
  target: DataTarget;
  relationshipName: string;
  remoteSchemaName: string;
  lhs_fields: string[];
  remote_field: RemoteField;
}

export interface DbToDbRelationship {
  target: DataTarget;
  relationshipName: string;
  relationshipType: 'object' | 'array';
  remoteDbName: string;
  fieldMapping: Record<string, string>;
}

type GDCSourcePrefix = string;

type SupportedDataSourcesPrefix =
  | 'mysql_'
  | 'mssql_'
  | 'bigquery_'
  | 'citus_'
  | 'pg_'
  | `${GDCSourcePrefix}_`;

export interface TableRelationship {
  name: string;
  comment: string;
  type: 'object' | 'array';
  from: {
    table: string;
    column: string[];
  };
  to: {
    table: string;
    column: string[];
  };
}

export type AllMetadataQueries =
  `${SupportedDataSourcesPrefix}${MetadataQueryType}`;

// TODO: these could be more strongly typed
export type allowedMetadataTypes =
  | (typeof allowedMetadataTypesArr)[number]
  | AllMetadataQueries
  | MetadataQueryType
  | OpenTelemetryQueries;
