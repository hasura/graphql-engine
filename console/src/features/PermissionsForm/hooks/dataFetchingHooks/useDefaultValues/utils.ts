import isEqual from 'lodash.isequal';
import { TableColumn } from '@/features/DataSource';

import type {
  DeletePermissionDefinition,
  InsertPermissionDefinition,
  MetadataTable,
  Permission,
  SelectPermissionDefinition,
  UpdatePermissionDefinition,
} from '@/features/MetadataAPI';

import { isPermission, keyToPermission, permissionToKey } from '../utils';
import { createDefaultValues } from '../../../components/RowPermissionsBuilder';

import type { QueryType } from '../../../types';

export const getCheckType = (
  check?: Record<string, unknown> | null
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
  currentQueryPermissions?: Record<string, any>;
}

export const getRowCount = ({ currentQueryPermissions }: GetRowCountArgs) => {
  return `${currentQueryPermissions?.limit ?? 0}`;
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

  return set.map(([columnName, value]) => {
    return {
      columnName,
      presetType: value.startsWith('x-hasura')
        ? 'from session variable'
        : 'static',
      value,
    };
  });
};

const getColumns = (
  permissionColumns: string[],
  tableColumns: TableColumn[]
) => {
  return tableColumns.reduce<Record<string, boolean>>((acc, each) => {
    const columnIncluded = permissionColumns?.includes(each.name);
    acc[each.name] = !!columnIncluded;
    return acc;
  }, {});
};

export const getAllRowChecks = (
  currentQuery: QueryType,
  allChecks: Array<{ queryType: QueryType; value: any }> = []
) => {
  return allChecks
    .filter(({ queryType }) => queryType !== currentQuery)
    .map(({ queryType, value }) => {
      if (['insert', 'update'].includes(queryType)) {
        return { queryType, value: JSON.stringify(value.check || {}) };
      }

      return {
        queryType,
        value: JSON.stringify(value.filter || {}),
      };
    });
};

export interface UseDefaultValuesArgs {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: QueryType;
}

export const getRowPermissionsForAllOtherQueriesMatchingSelectedRole = (
  selectedQuery: QueryType,
  selectedRole: string,
  table?: MetadataTable
) => {
  const res = Object.entries(table || {}).reduce<
    Array<{ queryType: QueryType; value: any }>
  >((acc, [key, value]) => {
    const props = { key, value };

    // check object key of metadata is a permission
    if (isPermission(props)) {
      // add each role from each permission to the set
      props.value.forEach(permission => {
        if (permission.role === selectedRole) {
          acc.push({
            queryType: keyToPermission[props.key],
            value: permission.permission,
          });
        }
      });
    }

    return acc;
  }, []);

  return getAllRowChecks(selectedQuery, res);
};

export const createPermission = {
  insert: (
    permission: InsertPermissionDefinition,
    tableColumns: TableColumn[]
  ) => {
    const check = JSON.stringify(permission.check) || '';
    const checkType = getCheckType(permission.check);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });
    const columns = getColumns(permission?.columns || [], tableColumns);
    const backendOnly: boolean = permission?.backend_only || false;

    return { check, checkType, presets, columns, backendOnly };
  },
  select: (
    permission: SelectPermissionDefinition,
    tableColumns: TableColumn[],
    schema: any
  ) => {
    const { filter, operators } = createDefaultValues({
      tableName: 'Artist',
      existingPermission: permission.filter,
      schema,
    });

    const filterType = getCheckType(permission?.filter);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });
    const columns = getColumns(permission?.columns || [], tableColumns);
    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });
    const aggregationEnabled: boolean = permission?.allow_aggregations || false;

    const selectPermissions = {
      filter,
      filterType,
      presets,
      columns,
      rowCount,
      aggregationEnabled,
      operators,
    };

    if (rowCount) {
      selectPermissions.rowCount = rowCount;
    }

    return selectPermissions;
  },
  update: (
    permission: UpdatePermissionDefinition,
    tableColumns: TableColumn[]
  ) => {
    const check = JSON.stringify(permission?.check) || '';
    const filter = JSON.stringify(permission?.filter) || '';
    const checkType = getCheckType(permission?.check);
    const filterType = getCheckType(permission?.filter);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });
    const columns = getColumns(permission?.columns || [], tableColumns);
    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });

    return { check, checkType, filter, filterType, presets, columns, rowCount };
  },
  delete: (permission: DeletePermissionDefinition) => {
    const filter = JSON.stringify(permission?.filter) || '';
    const filterType = getCheckType(permission?.filter);
    const presets = getPresets({
      currentQueryPermissions: permission,
    });

    const rowCount = getRowCount({
      currentQueryPermissions: permission,
    });

    return { filter, filterType, presets, rowCount };
  },
};

interface GetCurrentPermissionArgs {
  table?: MetadataTable;
  roleName: string;
  queryType: QueryType;
}

export const getCurrentPermission = ({
  table,
  roleName,
  queryType,
}: GetCurrentPermissionArgs) => {
  const key = permissionToKey[queryType];
  const currentPermission = table?.[key] as Permission[];

  const currentPermissionsForSelectedRole = currentPermission?.find(
    permission => permission.role === roleName
  );

  if (currentPermissionsForSelectedRole) {
    return {
      queryType,
      permission: currentPermissionsForSelectedRole?.permission,
    };
  }

  return {
    queryType,
    permission: {},
  };
};

interface ObjArgs {
  queryType: QueryType;
  selectedTable: MetadataTable;
  tableColumns: TableColumn[];
  roleName: string;
  schema: any;
}

export const createPermissionsObject = ({
  queryType,
  selectedTable,
  tableColumns,
  roleName,
  schema,
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
        tableColumns
      );
    case 'select':
      return createPermission.select(
        selectedPermission.permission as SelectPermissionDefinition,
        tableColumns,
        schema
      );
    case 'update':
      return createPermission.update(
        selectedPermission.permission,
        tableColumns
      );
    case 'delete':
      return createPermission.delete(selectedPermission.permission);
    default:
      throw new Error('Case not handled');
  }
};
