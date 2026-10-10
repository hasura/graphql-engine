import isEqual from 'lodash/isEqual';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import type { TableColumn } from '@hasura/metadata/data-source';
import { useQuery } from '@tanstack/react-query';
import {
  type Metadata,
  type Table,
  type DataQueryType,
  type AccessType,
  keyToPermission,
} from '@hasura/shared/types';
import { useMetadataHelpers } from '@hasura/metadata/api';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { isPermission } from '../../utils';

export interface RolePermission {
  roleName: string;
  isNewRole: boolean;
  permissionTypes: {
    permissionType: DataQueryType;
    access: AccessType;
  }[];
  bulkSelect: {
    isSelectable: boolean;
    isDisabled: boolean;
  };
}

const supportedQueries: DataQueryType[] = [
  'insert',
  'select',
  'update',
  'delete',
];

export const getAllowedFilterKeys = (
  query: 'insert' | 'select' | 'update' | 'delete',
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
  QueryType: DataQueryType;
  permission: any;
  // permission: Permission['permission'];
  tableColumns: TableColumn[];
};

const getAccessType = ({
  QueryType,
  permission,
  tableColumns,
}: GetAccessTypeArgs): AccessType => {
  const filterKeys = getAllowedFilterKeys(QueryType);
  const checkColumns = QueryType !== 'delete';
  // const checkComputedFields = QueryType === 'select';

  // if any permissions are set for any of the filter keys then
  // the user only has partial access to that QueryType
  const hasRowPermissionsSet = !filterKeys.every(
    (key) => JSON.stringify(permission[key]) === '{}',
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
    (source) => source.name === dataSourceName,
  );

  if (!currentMetadataSource)
    throw Error(`useRolePermissions.metadataSource not found`);

  const trackedTables = currentMetadataSource.tables;

  // find selected table
  return trackedTables.find((trackedTable) =>
    isEqual(trackedTable.table, table),
  );
};

type SupportedQueriesObject = Partial<Record<DataQueryType, AccessType>>;

const createSupportedQueryObject = (access: AccessType) =>
  supportedQueries.reduce<SupportedQueriesObject>((acc, supportedQuery) => {
    acc[supportedQuery] = access;
    return acc;
  }, {});

type CreateRoleTableDataArgs = {
  metadataTable: any;
  tableColumns?: TableColumn[];
  allRoles: string[];
};

type RoleToPermissionsMap = Record<
  string,
  Partial<Record<DataQueryType, AccessType>>
>;

const createRoleTableData = ({
  metadataTable,
  tableColumns,
  allRoles,
}: CreateRoleTableDataArgs): RolePermission[] => {
  if (!metadataTable) return [];
  // create object with key of role
  // and value describing permissions attached to that role
  const roleToPermissionsMap = Object.entries(
    metadataTable,
  ).reduce<RoleToPermissionsMap>((acc, [key, value]) => {
    const props = { key, value };
    // check if metadata key is related to permissions
    if (isPermission(props)) {
      const QueryType = keyToPermission[props.key];

      props.value.forEach((permissionObject) => {
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
    {},
  );
  // create the array that has the relevant information for each row of the table
  const permissions = Object.entries(allRolesToPermissionsMap).map(
    ([roleName, permission]) => {
      const permissionEntries = Object.entries(permission) as [
        DataQueryType,
        AccessType,
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
    },
  );

  // add admin row
  // and row for adding a new role
  const finalPermissions = [
    {
      roleName: 'admin',
      isNewRole: false,
      permissionTypes: Object.entries(
        createSupportedQueryObject('fullAccess'),
      ).map(([key, value]) => ({
        permissionType: key as DataQueryType,
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
        createSupportedQueryObject('noAccess'),
      ).map(([key, value]) => ({
        permissionType: key as DataQueryType,
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
  table: Table;
};

export function permissionsTableKey({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) {
  return [dataSourceName, 'permissions', table];
}

export const useRolePermissions = ({
  dataSourceName,
  table,
}: UseRolePermissionsArgs) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { fetchMetadata } = useMetadataHelpers();

  return useQuery<
    { supportedQueries: DataQueryType[]; rolePermissions: RolePermission[] },
    Error
  >({
    queryKey: permissionsTableKey({
      dataSourceName,
      table,
    }),
    queryFn: async () => {
      const meta = await fetchMetadata();
      const source = meta.metadata.sources.find(
        (s) => s.name === dataSourceName,
      );
      if (!source) {
        throw new Error('Source not found');
      }

      const databaseMethods = getDatabaseMethods(source.kind);

      // get table columns for metadata table from db introspection
      const tableColumns =
        await databaseMethods.introspection.getTableColumnInfos({
          dataSourceName: dataSourceName,
          table,
          endpoints,
          fetchJson,
        });
      const roles = MetadataSelectors.getRoles(meta.metadata);

      // find the specific metadata table
      const metadataTable = getMetadataTable({
        metadata: meta,
        dataSourceName,
        table,
      });

      // // extract the permissions data in the format required for the table
      const rolePermissions = createRoleTableData({
        metadataTable,
        tableColumns,
        allRoles: roles,
      });

      return { rolePermissions, supportedQueries };
    },
    refetchOnWindowFocus: false,
  });
};
