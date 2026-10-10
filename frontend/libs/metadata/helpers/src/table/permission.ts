import {
  DATA_QUERY_TYPES,
  DataQueryType,
  DeletePermission,
  InsertPermission,
  MetadataTable,
  SelectPermission,
  UpdatePermission,
} from '@hasura/shared/types';

export function isDataQueryType(input: unknown): input is DataQueryType {
  return (
    typeof input === 'string' &&
    DATA_QUERY_TYPES.includes(input as DataQueryType)
  );
}

export type TablePermissionWithType =
  | {
      type: 'select';
      definition: SelectPermission;
    }
  | {
      type: 'insert';
      definition: InsertPermission;
    }
  | {
      type: 'update';
      definition: UpdatePermission;
    }
  | {
      type: 'delete';
      definition: DeletePermission;
    };

export function flattenTablePermissions(
  metadataTable: MetadataTable | null | undefined,
): TablePermissionWithType[] {
  if (!metadataTable) {
    return [];
  }

  return [
    ...(metadataTable.select_permissions?.map((p) => ({
      type: 'select' as const,
      definition: p,
    })) ?? []),
    ...(metadataTable.insert_permissions?.map((p) => ({
      type: 'insert' as const,
      definition: p,
    })) ?? []),
    ...(metadataTable.update_permissions?.map((p) => ({
      type: 'update' as const,
      definition: p,
    })) ?? []),
    ...(metadataTable.delete_permissions?.map((p) => ({
      type: 'delete' as const,
      definition: p,
    })) ?? []),
  ];
}
