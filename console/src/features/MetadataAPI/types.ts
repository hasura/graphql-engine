import { HasuraMetadataV3 } from '@/metadata/types';
import { IntrospectionQuery } from 'graphql';

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
  'bulk',
] as const;

export type allowedMetadataTypes = typeof allowedMetadataTypesArr[number];
