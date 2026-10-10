import isEqual from 'lodash/isEqual';
import { TableColumn } from '@hasura/metadata/data-source';
import {
  type DeletePermissionDefinition,
  type InsertPermissionDefinition,
  type TablePermission,
  type SelectPermissionDefinition,
  type UpdatePermissionDefinition,
  type DataQueryType,
  type ComputedField,
  type MetadataTable,
  type Source,
  permissionToKey,
} from '@hasura/shared/types';
import { createDefaultValues } from '../../../../components/RowPermissionsBuilder';
import { PermissionsSchema } from '../../../../../schema';

export type DefaultPermissionValues = PermissionsSchema & {
  operators?: Record<string, unknown>;
};

export const getCheckType = (
  check?: Record<string, unknown> | null,
): 'none' | 'no_checks' | 'custom' => {
  if (!check) {
    return 'none';
  }

  if (isEqual(check, {})) {
    return 'no_checks';
  }

  return 'custom';
};

interface GetRowCountArgs {
  currentQueryPermissions?: SelectPermissionDefinition;
}

export const getRowCount = ({ currentQueryPermissions }: GetRowCountArgs) => {
  return currentQueryPermissions?.limit
    ? String(currentQueryPermissions.limit)
    : '';
};

interface GetCheckArgs {
  currentQueryPermissions?: Record<string, any>;
  type: 'check' | 'filter';
}

export const getCheck = ({ currentQueryPermissions, type }: GetCheckArgs) => {
  const check = currentQueryPermissions?.[type];
  return check ? JSON.stringify(check) : '';
};

interface GetPresetArgs {
  currentQueryPermissions?: Record<string, any>;
}

export const getPresets = ({ currentQueryPermissions }: GetPresetArgs) => {
  const set = Object.entries(currentQueryPermissions?.set || {}) as Array<
    [string, string]
  >;

  return set.map(([columnName, columnValue]) => {
    return {
      columnName,
      presetType:
        typeof columnValue === 'string' &&
        columnValue.toLowerCase().startsWith('x-hasura')
          ? 'from session variable'
          : 'static',
      columnValue,
    };
  });
};

const getColumns = (
  permissionColumns: string[] | '*',
  tableColumns: TableColumn[],
) => {
  return tableColumns.reduce<Record<string, boolean>>((acc, each) => {
    const columnIncluded =
      permissionColumns === '*' ||
      (Array.isArray(permissionColumns) &&
        permissionColumns.includes(each.name));
    acc[each.name] = !!columnIncluded;
    return acc;
  }, {});
};

export const createPermission = {
  insert: (
    permission: InsertPermissionDefinition,
    tableColumns: TableColumn[],
  ) => {
    const check = permission.check || {};
    const checkType = getCheckType(permission.check);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });
    const columns = getColumns(permission?.columns ?? [], tableColumns);
    const backendOnly: boolean = permission?.backend_only || false;

    return {
      // Needs to be cast to const for the zod schema to accept it as a literal for the discriminated union
      queryType: 'insert' as const,
      check,
      checkType,
      presets,
      columns,
      backendOnly,
      comment: permission.comment ?? '',
    };
  },
  select: (
    permission: SelectPermissionDefinition,
    tableColumns: TableColumn[],
    tableName: string,
    metadataSource: Source | undefined,
  ) => {
    const { filter, operators: ops } = createDefaultValues({
      tableName,
      existingPermission: permission.filter,
      tableColumns,
      sourceMetadata: metadataSource,
    });

    const filterType = getCheckType(permission?.filter);

    const columns = getColumns(permission?.columns || [], tableColumns);
    const computed_fields = permission?.computed_fields ?? [];

    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });
    const aggregationEnabled: boolean = permission?.allow_aggregations || false;
    const customRootFieldEnabled =
      Boolean(permission.query_root_fields?.length) ||
      Boolean(permission.subscription_root_fields?.length);

    const selectPermissions: Partial<DefaultPermissionValues> = {
      // Needs to be cast to const for the zod schema to accept it as a literal for the discriminated union
      queryType: 'select' as const,
      filter,
      filterType,
      columns,
      computed_fields,
      rowCount,
      aggregationEnabled,
      operators: ops,
      customRootFieldEnabled,
      query_root_fields: permission.query_root_fields ?? [],
      subscription_root_fields: permission.subscription_root_fields ?? [],
      comment: permission.comment ?? '',
    };

    if (rowCount) {
      selectPermissions.rowCount = rowCount;
    }

    return selectPermissions;
  },
  update: (
    permission: UpdatePermissionDefinition,
    tableColumns: TableColumn[],
  ) => {
    const check = permission?.check || {};
    const filter = permission?.filter || {};
    const checkType = getCheckType(permission?.check);
    const filterType = getCheckType(permission?.filter);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });
    const columns = getColumns(permission?.columns || [], tableColumns);
    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });

    return {
      // Needs to be cast to const for the zod schema to accept it as a literal for the discriminated union
      queryType: 'update' as const,
      check,
      checkType,
      filter,
      filterType,
      presets,
      columns,
      rowCount,
      comment: permission.comment ?? '',
    };
  },
  delete: (permission: DeletePermissionDefinition) => {
    const filter = permission?.filter || {};
    const filterType = getCheckType(permission?.filter);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });

    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });

    return {
      // Needs to be cast to const for the zod schema to accept it as a literal for the discriminated union
      queryType: 'delete' as const,
      filter,
      filterType,
      presets,
      rowCount,
    };
  },
};

interface GetCurrentPermissionArgs {
  table?: MetadataTable;
  roleName: string;
  queryType: DataQueryType;
}

export const getCurrentPermission = ({
  table,
  roleName,
  queryType,
}: GetCurrentPermissionArgs) => {
  const key = permissionToKey[queryType];
  const currentPermission = table?.[key] as TablePermission[];

  const currentPermissionsForSelectedRole = currentPermission?.find(
    (permission) => permission.role === roleName,
  );

  if (currentPermissionsForSelectedRole) {
    return {
      queryType,
      permission: {
        ...currentPermissionsForSelectedRole?.permission,
        comment: currentPermissionsForSelectedRole?.comment,
      },
    };
  }

  return {
    queryType,
    permission: {},
  };
};

interface ObjArgs {
  queryType: DataQueryType;
  selectedTable: MetadataTable;
  tableColumns: TableColumn[];
  tableComputedFields: ComputedField[];
  roleName: string;
  tableName: string;
  metadataSource: Source | undefined;
}

export const createPermissionsObject = ({
  queryType,
  selectedTable,
  tableColumns,
  roleName,
  tableName,
  metadataSource,
}: ObjArgs) => {
  const selectedPermission = getCurrentPermission({
    table: selectedTable,
    queryType,
    roleName,
  });

  switch (selectedPermission.queryType) {
    case 'insert':
      return createPermission.insert(
        selectedPermission.permission,
        tableColumns,
      );
    case 'select':
      return createPermission.select(
        selectedPermission.permission as SelectPermissionDefinition,
        tableColumns,
        tableName,
        // selectedTable.configuration,
        metadataSource,
      );
    case 'update':
      return createPermission.update(
        selectedPermission.permission,
        tableColumns,
      );
    case 'delete':
      return createPermission.delete(selectedPermission.permission);
    default:
      throw new Error('Case not handled');
  }
};
