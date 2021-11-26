import { dataSource, Operations } from '@/dataSources';
import {
  ComputedField,
  // Table,
  TableColumn,
} from '@/dataSources/types';
import { useAllFunctions, useRoles, useSchemaList, useTables } from '@/hooks';

export type RolePermissions = {
  [role: string]: {
    [query in 'insert' | 'select' | 'update' | 'delete']: {
      columns: (string | '*')[];
      computed_fields: (string | '*')[];
    } & {
      [key in 'check' | 'filter']: Record<string, any>;
    };
  };
};

export const getAllowedFilterKeys = (
  query: 'insert' | 'select' | 'update' | 'delete'
): ('check' | 'filter')[] => {
  switch (query) {
    case 'insert':
      return ['check'];
    case 'update':
      return ['filter', 'check'];
    default:
      return ['filter'];
  }
};

export const getRolePermission = (
  role: 'admin' | string,
  rolePermissions: RolePermissions,
  query: 'insert' | 'select' | 'update' | 'delete',
  schemaColumns: TableColumn[],
  computedFields: { scalar: ComputedField[] }
): 'fullAccess' | 'partialAccess' | 'noAccess' => {
  if (role === 'admin') {
    return 'fullAccess';
  }

  if (!rolePermissions[role]) {
    return 'noAccess';
  }

  const permissions = rolePermissions[role][query];
  if (!permissions) {
    return 'noAccess';
  }

  const filterKeys = getAllowedFilterKeys(query);
  const checkColumns = query !== 'delete';
  const checkComputedFields = query === 'select';

  if (!filterKeys.every(key => JSON.stringify(permissions[key]) === '{}')) {
    return 'partialAccess';
  }

  if (
    checkColumns &&
    (!permissions.columns ||
      (!permissions.columns.includes('*') &&
        permissions.columns.length !== schemaColumns.length))
  ) {
    return 'partialAccess';
  }

  if (
    checkComputedFields &&
    computedFields.scalar.length &&
    (!permissions.computed_fields ||
      (permissions.computed_fields.includes('*') &&
        permissions.computed_fields.length !== computedFields.scalar.length))
  ) {
    return 'partialAccess';
  }

  return 'fullAccess';
};

interface RolePermission {
  roleName: string;
  isNewRole: boolean;
  permissionTypes: {
    permissionType: Operations;
    access: 'fullAccess' | 'partialAccess' | 'noAccess';
  }[];
  bulkSelect: {
    isSelectable: boolean;
    isDisabled: boolean;
  };
}

interface UseRolePermissionsArgs {
  schemaName: string;
  tableName: string;
}

export const useRolePermissions = ({
  tableName,
  schemaName,
}: UseRolePermissionsArgs) => {
  const { data: schemaList } = useSchemaList();
  const { data: tables } = useTables(
    { schemas: schemaList! },
    { enabled: !!schemaList }
  );
  const currentTableSchema = tables?.find(
    ({ table_schema, table_name }) =>
      table_schema === schemaName && table_name === tableName
  );
  const { data: allFunctions } = useAllFunctions(schemaList!, {
    enabled: !!schemaList,
  });
  const { data: roles } = useRoles();

  if (!currentTableSchema || !allFunctions) {
    return { supportedQueries: [], rolePermissions: [] };
  }

  const currentRolePermissions = currentTableSchema.permissions.reduce(
    (acc, p) => {
      acc[p.role_name] = p.permissions;
      return acc;
    },
    {} as Record<string, any>
  );

  const supportedQueries = dataSource.getTableSupportedQueries(
    currentTableSchema
  );

  const groupedComputedFields = dataSource.getGroupedTableComputedFields(
    currentTableSchema,
    allFunctions
  );

  const currentRoles = roles.map(roleName => ({
    roleName,
    isNewRole: false,
  }));

  const roleList = [
    { roleName: 'admin', isNewRole: false },
    ...currentRoles,
    { roleName: '', isNewRole: true },
  ];

  const rolePermissions: RolePermission[] = roleList.map(
    ({ roleName, isNewRole }) => ({
      roleName,
      isNewRole,
      permissionTypes: supportedQueries.map(queryType => ({
        permissionType: queryType,
        access: getRolePermission(
          roleName,
          currentRolePermissions,
          queryType,
          currentTableSchema!.columns,
          groupedComputedFields
        ),
      })),
      bulkSelect: {
        isSelectable: roleName !== 'admin' && !isNewRole,
        isDisabled: !Object.keys(currentRolePermissions).includes(roleName),
      },
    })
  );

  return { supportedQueries, rolePermissions };
};
