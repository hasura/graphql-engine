import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
import { RemoteSchema } from '@hasura/shared/types';
import { getDropRemoteSchemaPermissionQuery } from './useDropRemoteSchemaPermissions';
import { TMigrationSingleQuery } from '../../api';
import { useErrorNotification } from '../notification';

type DropRemoteSchemaPermissionMultipleRolesArgs = {
  currentRemoteSchema: RemoteSchema;
  roles: string[];
};

export const useDropRemoteSchemaPermissionMultipleRoles = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      args: DropRemoteSchemaPermissionMultipleRolesArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const currentPermissions = args.currentRemoteSchema.permissions;
      if (!currentPermissions?.length || !args.roles.length) {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: 'No permission or role to be dropped',
        });

        return;
      }

      const isOk = getConfirmation(
        `This will remove permissions for these roles: ${args.roles.join(', ')}`,
      );
      if (!isOk) return;

      const queries: TMigrationSingleQuery[] = [];

      args.roles.forEach((role) => {
        const currentRolePermission = currentPermissions.find((el) => {
          return el.role === role;
        });
        if (!currentRolePermission) {
          return;
        }

        const upQuery = getDropRemoteSchemaPermissionQuery(
          role,
          args.currentRemoteSchema.name,
        );
        queries.push(upQuery);
      });

      return mutation.mutate(
        {
          query: {
            type: 'bulk',
            args: queries,
          },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Removing permission failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Permission removed successfully',
            });

            if (onSuccess) onSuccess();
          },
        },
      );
    },
    [mutation],
  );

  return {
    ...mutation,
    mutate,
  };
};
