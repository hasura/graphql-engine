import { Table } from '@/features/hasura-metadata-types';

interface BaseParams {
  dataSourceName: string;
  table: Table;
}

export const generateQueryKeys = {
  metadata: () => ['export_metadata'],
  fkConstraints: (params: BaseParams) =>
    [
      'dal-introspection',
      params.dataSourceName,
      params.table,
      'fkConstraints',
    ] as const,

  columns: (params: BaseParams) =>
    [
      'dal-introspection',
      params.dataSourceName,
      params.table,
      'columns',
    ] as const,

  allRelationships: (params: BaseParams) =>
    [
      'export_metadata',
      params.dataSourceName,
      params.table,
      'database_relationships',
    ] as const,

  relationship: (params: BaseParams & { relationshipName: string }) =>
    [
      'export_metadata',
      params.dataSourceName,
      params.table,
      'database_relationships',
      params.relationshipName,
    ] as const,
};
