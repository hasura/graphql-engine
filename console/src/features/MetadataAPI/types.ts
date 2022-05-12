import { HasuraMetadataV3 } from '@/metadata/types';
import { MetadataQueryType } from '@/metadata/queryUtils';
import { IntrospectionQuery } from 'graphql';
import { RemoteField } from '../RemoteRelationships/RemoteSchemaRelationships/types';
import { DataTarget } from '../Datasources';

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

type SupportedDataSourcesPrefix =
  | 'mysql_'
  | 'mssql_'
  | 'bigquery_'
  | 'citus_'
  | 'pg_';

export type AllMetadataQueries = `${SupportedDataSourcesPrefix}${MetadataQueryType}`;

export type allowedMetadataTypes =
  | typeof allowedMetadataTypesArr[number]
  | AllMetadataQueries;
