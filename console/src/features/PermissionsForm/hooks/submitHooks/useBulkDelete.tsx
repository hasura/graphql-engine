import { useQueryClient } from 'react-query';
import { AxiosInstance } from 'axios';
import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { Permission, useMetadataMigration } from '@/features/MetadataAPI';

import { api } from '../../api';
import { QueryType } from '../../types';

interface GetMetadataTableArgs {
  dataSourceName: string;
  table: unknown;
  httpClient: AxiosInstance;
}

const getMetadataTable = async ({
  dataSourceName,
  table,
  httpClient,
}: GetMetadataTableArgs) => {
  // get all metadata
  const { metadata, resource_version } = await exportMetadata({ httpClient });

  // find current source
  const currentMetadataSource = metadata?.sources?.find(
    source => source.name === dataSourceName
  );

  if (!currentMetadataSource)
    throw Error(`useRolePermissions.metadataSource not found`);

  const trackedTables = currentMetadataSource.tables;

  // find selected table
  return {
    metadataTable: trackedTables.find(
      trackedTable =>
        JSON.stringify(trackedTable.table) === JSON.stringify(table)
    ),
    resourceVersion: resource_version,
  };
};

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

interface RoleList {
  roleName: string;
  queries: QueryType[];
}

const isPermission = (props: {
  key: string;
  value: any;
}): props is {
  key: typeof metadataPermissionKeys[number];
  value: Permission[];
} => props.key in keyToPermission;

interface Args {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
}

export const useBulkDeletePermissions = ({
  currentSource,
  dataSourceName,
  table,
}: Args) => {
  const {
    mutateAsync,
    isLoading: mutationLoading,
    isError: mutationError,
    ...rest
  } = useMetadataMigration();

  const httpClient = useHttpClient();
  const queryClient = useQueryClient();

  const submit = async (roles: string[]) => {
    const { metadataTable, resourceVersion } = await getMetadataTable({
      dataSourceName,
      table,
      httpClient,
    });

    const permissions = Object.entries(metadataTable || {}).reduce<
      Record<string, QueryType[]>
    >((acc, [key, value]) => {
      const props = { key, value };
      // check if metadata key is related to permissions
      if (isPermission(props)) {
        props.value.forEach(permissionObject => {
          // only add role if it is one of the selected roles for deletion
          if (roles.includes(permissionObject.role)) {
            if (!acc[permissionObject.role]) {
              acc[permissionObject.role] = [];
            }

            acc[permissionObject.role].push(keyToPermission[props.key]);
          }
        });
      }

      return acc;
    }, {});

    const roleList = Object.entries(permissions).reduce<RoleList[]>(
      (acc, [key, value]) => {
        acc.push({
          roleName: key,
          queries: value,
        });
        return acc;
      },
      []
    );

    const body = api.createBulkDeleteBody({
      source: currentSource,
      dataSourceName,
      table,
      resourceVersion,
      roleList,
    });

    await mutateAsync({
      query: body,
    });

    queryClient.invalidateQueries([dataSourceName, 'permissionsTable']);
  };

  const isLoading = mutationLoading;
  const isError = mutationError;

  return {
    submit,
    ...rest,
    isError,
    isLoading,
  };
};
