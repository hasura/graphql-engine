import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';
import type {
  BasePermission,
  MetadataTable,
  SupportedDriver,
  DataQueryType,
} from '@hasura/shared/types';
import { getDropPermissionQuery } from './useDropTablePermission';
import { TMigrationSingleQuery } from '../../api';

type DropTablePermissionMultipleRolesArgs = {
  tableSchema: MetadataTable;
  source: string;
  roles: string[];
  driver: SupportedDriver;
};

export const useDropTablePermissionMultipleRoles = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      {
        tableSchema,
        source,
        roles,
        driver,
      }: DropTablePermissionMultipleRolesArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const queryArgs: TMigrationSingleQuery[] = [];

      const addRoles = (
        operation: DataQueryType,
        permissions: BasePermission[] | undefined,
      ) => {
        permissions?.forEach((permission) => {
          if (!roles.includes(permission.role)) {
            return;
          }

          const deleteQuery = getDropPermissionQuery(operation, driver, {
            table: tableSchema.table,
            source,
            role: permission.role,
          });
          queryArgs.push(deleteQuery);
        });
      };

      addRoles('select', tableSchema.select_permissions);
      addRoles('insert', tableSchema.insert_permissions);
      addRoles('update', tableSchema.update_permissions);
      addRoles('delete', tableSchema.delete_permissions);

      if (!queryArgs.length) {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: `Roles [${roles.join(',')}] don't have permission to be removed`,
        });

        return;
      }

      return mutation.mutate(
        {
          query: { type: 'bulk', args: queryArgs },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Removing permissions failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Permissions removed',
            });

            onSuccess?.();
          },
        },
      );
    },
    [mutation, showErrorNotification],
  );

  return {
    ...mutation,
    mutate,
  };
};
