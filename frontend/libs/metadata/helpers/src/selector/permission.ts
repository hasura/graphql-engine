import {
  DataQueryType,
  MetadataTable,
  Table,
  TablePermission,
} from '@hasura/shared/types';
import { areTablesEqual } from '../table/predicate';

export function findPermissionByQueryAndRole(
  tableSchema: MetadataTable,
  queryType: DataQueryType,
  role: string,
): TablePermission | undefined {
  switch (queryType) {
    case 'insert':
      return tableSchema.insert_permissions?.find((p) => p.role === role);
    case 'select':
      return tableSchema.select_permissions?.find((p) => p.role === role);
    case 'update':
      return tableSchema.update_permissions?.find((p) => p.role === role);
    case 'delete':
      return tableSchema.delete_permissions?.find((p) => p.role === role);
  }
}

export function findPermissionFromTables(
  tables: MetadataTable[],
  table: Table,
  queryType: DataQueryType,
  role: string,
): TablePermission | undefined {
  const metadataTable = tables.find((t) => areTablesEqual(t.table, table));
  if (!metadataTable) {
    return undefined;
  }

  return findPermissionByQueryAndRole(metadataTable, queryType, role);
}
