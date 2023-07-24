import isEqual from 'lodash/isEqual';
import { DataSource, exportMetadata } from '../../../DataSource';
import type { TableColumn } from '../../../DataSource';

import { useQuery } from 'react-query';
import { useHttpClient } from '../../../Network';
import { MetadataTable, Metadata } from '../../../hasura-metadata-types';
import { keyToPermission, metadataPermissionKeys } from '../../utils';

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
  metadata?: Metadata;
};

const getMetadataTable = ({
  metadata,
  dataSourceName,
  table,
}: GetMetadataTableArgs) => {
  // find current source
  const currentMetadataSource = metadata?.metadata?.sources?.find(
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
  key: (typeof metadataPermissionKeys)[number];
  value: any[];
  // value: Permission[];
} => props.key in keyToPermission;

type CreateRoleTableDataArgs = {
  metadataTable: any;
  tableColumns?: TableColumn[];
  allRoles: string[];
};

type RoleToPermissionsMap = Record<string, Partial<Record<QueryType, Access>>>;

const createRoleTableData = ({
  metadataTable,
  tableColumns,
  allRoles,
}: CreateRoleTableDataArgs): RolePermission[] => {
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

  const allRolesToPermissionsMap = allRoles.reduce<RoleToPermissionsMap>(
    (acc, role) => {
      return {
        ...acc,
        [role]: roleToPermissionsMap[role] ?? {
          insert: 'noAccess',
          select: 'noAccess',
          update: 'noAccess',
          delete: 'noAccess',
        },
      };
    },
    {}
  );
  // create the array that has the relevant information for each row of the table
  const permissions = Object.entries(allRolesToPermissionsMap).map(
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

type PermKeys = Pick<
  MetadataTable,
  | 'update_permissions'
  | 'select_permissions'
  | 'delete_permissions'
  | 'insert_permissions'
>;
const permKeys: Array<keyof PermKeys> = [
  'insert_permissions',
  'update_permissions',
  'select_permissions',
  'delete_permissions',
];

const getRoles = (m: Metadata) => {
  if (!m) return null;

  const { metadata } = m;

  const actions = metadata.actions;
  const tableEntries: MetadataTable[] = metadata.sources.reduce<
    MetadataTable[]
  >((acc, source) => {
    return [...acc, ...source.tables];
  }, []);
  const inheritedRoles = metadata.inherited_roles;
  const remoteSchemas = metadata.remote_schemas;
  const allowlists = metadata.allowlist;
  const securitySettings = {
    api_limits: metadata.api_limits,
    graphql_schema_introspection: metadata.graphql_schema_introspection,
  };
  const roleNames: string[] = [];

  tableEntries?.forEach(table =>
    permKeys.forEach(key =>
      table[key]?.forEach(({ role }: { role: string }) => roleNames.push(role))
    )
  );

  actions?.forEach(action =>
    action.permissions?.forEach(p => roleNames.push(p.role))
  );

  remoteSchemas?.forEach(remoteSchema => {
    remoteSchema?.permissions?.forEach(p => roleNames.push(p.role));
  });

  allowlists?.forEach(allowlist => {
    if (allowlist?.scope?.global === false) {
      allowlist?.scope?.roles?.forEach(role => roleNames.push(role));
    }
  });

  inheritedRoles?.forEach(role => roleNames.push(role.role_name));

  Object.entries(securitySettings?.api_limits ?? {}).forEach(
    ([limit, value]) => {
      if (limit !== 'disabled' && typeof value !== 'boolean') {
        Object.keys(value?.per_role ?? {}).forEach(role =>
          roleNames.push(role)
        );
      }
    }
  );

  securitySettings?.graphql_schema_introspection?.disabled_for_roles.forEach(
    role => roleNames.push(role)
  );

  return Array.from(new Set(roleNames));
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
    queryKey: [dataSourceName, 'permissionsTable', table],
    queryFn: async () => {
      const metadata = await exportMetadata({ httpClient });
      // get table columns for metadata table from db introspection
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      // find the specific metadata table
      const metadataTable = getMetadataTable({
        metadata,
        dataSourceName,
        table,
      });

      // get all roles
      const roles = getRoles(metadata);

      // // extract the permissions data in the format required for the table
      const rolePermissions = createRoleTableData({
        metadataTable,
        tableColumns,
        allRoles: roles ?? [],
      });

      return { rolePermissions, supportedQueries };
    },
    refetchOnWindowFocus: false,
  });
};
