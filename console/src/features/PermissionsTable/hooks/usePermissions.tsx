import { AxiosInstance } from 'axios';
import isEqual from 'lodash.isequal';
import { DataSource, exportMetadata } from '@/features/DataSource';
import type { TableColumn } from '@/features/DataSource';

import { useQuery } from 'react-query';
import { useHttpClient } from '@/features/Network';

interface RolePermission {
  roleName: string;
  isNewRole: boolean;
  permissionTypes: {
    permissionType: QueryType;
    access: Access;
  }[];
  bulkSelect: {
    isSelectable: boolean;
    isDisabled: boolean;
  };
}

const metadataPermissionKeys = [
  'insert_permissions',
  'select_permissions',
  'update_permissions',
  'delete_permissions',
] as const;

export const keyToPermission = {
  insert_permissions: 'insert',
  select_permissions: 'select',
  update_permissions: 'update',
  delete_permissions: 'delete',
} as const;

type QueryType = 'insert' | 'select' | 'update' | 'delete';
type Access = 'fullAccess' | 'partialAccess' | 'noAccess';

const supportedQueries: QueryType[] = ['insert', 'select', 'update', 'delete'];

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

type GetAccessTypeArgs = {
  QueryType: QueryType;
  permission: any;
  // permission: Permission['permission'];
  tableColumns: TableColumn[];
};

const getAccessType = ({
  QueryType,
  permission,
  tableColumns,
}: GetAccessTypeArgs): Access => {
  const filterKeys = getAllowedFilterKeys(QueryType);
  const checkColumns = QueryType !== 'delete';
  // const checkComputedFields = QueryType === 'select';

  // if any permissions are set for any of the filter keys then
  // the user only has partial access to that QueryType
  const hasRowPermissionsSet = !filterKeys.every(
    key => JSON.stringify(permission[key]) === '{}'
  );
  if (hasRowPermissionsSet) {
    return 'partialAccess';
  }

  // unless all columns are selected
  // the user only has partial access to that QueryType
  const noColumnsChecked = !permission.columns;
  const allColumnsChecked =
    permission.columns?.includes('*') ||
    permission.columns?.length === tableColumns.length;

  const hasLimitedAccessToColumns =
    checkColumns && (noColumnsChecked || !allColumnsChecked);
  if (hasLimitedAccessToColumns) {
    return 'partialAccess';
  }

  return 'fullAccess';
};

type GetMetadataTableArgs = {
  dataSourceName: string;
  table: unknown;
  httpClient: AxiosInstance;
};

const getMetadataTable = async ({
  httpClient,
  dataSourceName,
  table,
}: GetMetadataTableArgs) => {
  // get all metadata
  const { metadata } = await exportMetadata({ httpClient });

  // find current source
  const currentMetadataSource = metadata?.sources?.find(
    source => source.name === dataSourceName
  );

  if (!currentMetadataSource)
    throw Error(`useRolePermissions.metadataSource not found`);

  const trackedTables = currentMetadataSource.tables;

  // find selected table
  return trackedTables.find(trackedTable => isEqual(trackedTable.table, table));
};

type SupportedQueriesObject = Partial<Record<QueryType, Access>>;

const createSupportedQueryObject = (access: Access) =>
  supportedQueries.reduce<SupportedQueriesObject>((acc, supportedQuery) => {
    acc[supportedQuery] = access;
    return acc;
  }, {});

const isPermission = (props: {
  key: string;
  value: any;
}): props is {
  key: typeof metadataPermissionKeys[number];
  value: any[];
  // value: Permission[];
} => props.key in keyToPermission;

type CreateRoleTableDataArgs = {
  metadataTable: any;
  tableColumns?: TableColumn[];
};

type RoleToPermissionsMap = Record<string, Partial<Record<QueryType, Access>>>;

const createRoleTableData = async ({
  metadataTable,
  tableColumns,
}: CreateRoleTableDataArgs): Promise<RolePermission[]> => {
  if (!metadataTable) return [];
  // create object with key of role
  // and value describing permissions attached to that role
  const roleToPermissionsMap = Object.entries(
    metadataTable
  ).reduce<RoleToPermissionsMap>((acc, [key, value]) => {
    const props = { key, value };
    // check if metadata key is related to permissions
    if (isPermission(props)) {
      const QueryType = keyToPermission[props.key];

      props.value.forEach(permissionObject => {
        if (!acc[permissionObject.role]) {
          // add all supported queries to the object
          acc[permissionObject.role] = createSupportedQueryObject('noAccess');
        }
        // if permission exists on metadata for a particular QueryType
        // find out the access type for that QueryType
        // and replace the access type on the object
        acc[permissionObject.role][QueryType] = getAccessType({
          QueryType,
          permission: permissionObject.permission,
          tableColumns: tableColumns || [],
        });
      });
    }

    return acc;
  }, {});

  // create the array that has the relevant information for each row of the table
  const permissions = Object.entries(roleToPermissionsMap).map(
    ([roleName, permission]) => {
      const permissionEntries = Object.entries(permission) as [
        QueryType,
        Access
      ][];
      const permissionTypes = permissionEntries.map(([key, value]) => ({
        permissionType: key,
        access: value,
      }));

      const isNewRole = roleName === 'newRole';

      return {
        roleName: isNewRole ? '' : roleName,
        isNewRole,
        permissionTypes,
        bulkSelect: {
          isSelectable: roleName !== 'admin' && !isNewRole,
          isDisabled: false,
        },
      };
    }
  );

  // add admin row
  // and row for adding a new role
  const finalPermissions = [
    {
      roleName: 'admin',
      isNewRole: false,
      permissionTypes: Object.entries(
        createSupportedQueryObject('fullAccess')
      ).map(([key, value]) => ({
        permissionType: key as QueryType,
        access: value,
      })),
      bulkSelect: {
        isSelectable: false,
        isDisabled: false,
      },
    },
    ...permissions,
    {
      roleName: 'newRole',
      isNewRole: true,
      permissionTypes: Object.entries(
        createSupportedQueryObject('noAccess')
      ).map(([key, value]) => ({
        permissionType: key as QueryType,
        access: value,
      })),
      bulkSelect: {
        isSelectable: true,
        isDisabled: false,
      },
    },
  ];

  return finalPermissions;
};

type UseRolePermissionsArgs = {
  dataSourceName: string;
  table: unknown;
};

export const useRolePermissions = ({
  dataSourceName,
  table,
}: UseRolePermissionsArgs) => {
  const httpClient = useHttpClient();
  return useQuery<
    { supportedQueries: QueryType[]; rolePermissions: RolePermission[] },
    Error
  >({
    queryKey: [dataSourceName, 'permissionsTable'],
    queryFn: async () => {
      // find the specific metadata table
      const metadataTable = await getMetadataTable({
        httpClient,
        dataSourceName,
        table,
      });

      // get table columns for metadata table from db introspection
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      // // extract the permissions data in the format required for the table
      const rolePermissions = await createRoleTableData({
        metadataTable,
        tableColumns,
      });

      return { rolePermissions, supportedQueries };
    },
    refetchOnWindowFocus: false,
  });
};
