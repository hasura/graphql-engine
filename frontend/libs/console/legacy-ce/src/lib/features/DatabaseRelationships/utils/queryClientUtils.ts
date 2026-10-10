import { Table } from '@hasura/shared/types';

interface BaseParams {
  dataSourceName: string;
  table: Table;
}

interface ManyTablesParams {
  dataSourceName: string;
  tables: Table[] | undefined;
}

export const generateQueryKeys = {
  fkConstraints: (params: BaseParams) =>
    [
      'dal-introspection',
      params.dataSourceName,
      params.table,
      'fkConstraints',
    ] as const,
  manyFkConstraints: (params: ManyTablesParams) =>
    [
      'dal-introspection',
      params.dataSourceName,
      params.tables,
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

  suggestedRelationships: (params: BaseParams) =>
    ['suggested_relationships', params.dataSourceName, params.table] as const,

  relationship: (params: BaseParams & { relationshipName: string }) =>
    [
      'export_metadata',
      params.dataSourceName,
      params.table,
      'database_relationships',
      params.relationshipName,
    ] as const,
};
