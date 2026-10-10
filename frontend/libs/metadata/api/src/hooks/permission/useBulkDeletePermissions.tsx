import {
  keyToPermission,
  MetadataPermissionKey,
  metadataPermissionKeys,
  MetadataTable,
  QualifiedDataSource,
  type Table,
} from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import {
  useErrorNotification,
  useMetadataHelpers,
  useMetadataMigrationMutation,
} from '@hasura/metadata/api';
import { areTablesEqual, getDriverPrefix } from '@hasura/metadata/helpers';

interface Args {
  dataSourceName: string;
  table: Table | null;
}

export type BulkDropPermissionArgsInput = {
  tables: MetadataTable[];
  roles: string[];
  permissionKeys?: MetadataPermissionKey[];
  source: QualifiedDataSource;
};

export const getBulkDropPermissionArgs = ({
  tables,
  roles,
  permissionKeys,
  source,
}: BulkDropPermissionArgsInput) => {
  const prefix = getDriverPrefix(source.kind);

  return tables.flatMap((mt) => {
    const keys = permissionKeys?.length
      ? permissionKeys
      : metadataPermissionKeys;
    return keys.flatMap(
      (key) =>
        mt[key]
          ?.filter((permission) => roles.includes(permission.role))
          .map((permission) => {
            const queryType = keyToPermission[key];

            return {
              type: `${prefix}_drop_${queryType}_permission` as const,
              args: {
                table: mt.table,
                role: permission.role,
                source: source.name,
              },
            };
          }) ?? [],
    );
  });
};

export const useBulkDeletePermissions = ({ dataSourceName, table }: Args) => {
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  return useMetadataMigrationMutation(
    async ({
      roles,
      permissionKeys,
    }: {
      roles: string[];
      permissionKeys?: MetadataPermissionKey[];
    }) => {
      // get all metadata
      const { source, resource_version } = await fetchSource(dataSourceName);
      const metadataTables = table
        ? source.tables.filter((trackedTable) =>
            areTablesEqual(trackedTable.table, table),
          )
        : source.tables;

      return {
        type: 'bulk' as const,
        resource_version,
        args: getBulkDropPermissionArgs({
          tables: metadataTables,
          roles,
          permissionKeys,
          source,
        }),
      };
    },
    {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: 'Permissions successfully deleted',
        });
      },
      onError: (err) => {
        showErrorNotification({
          title: 'Deleting permissions failed!',
          error: err,
        });
      },
    },
  );
};
