import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast, useDestructiveConfirm } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export const getDropRemoteSchemaPermissionQuery = (
  role: string,
  remoteSchemaName: string,
) => {
  return {
    type: 'drop_remote_schema_permissions' as const,
    args: {
      remote_schema: remoteSchemaName,
      role,
    },
  };
};

type DropRemoteSchemaPermissionsArgs = {
  role: string;
  remoteSchemaName: string;
};
export const useDropRemoteSchemaPermissions = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();
  const destructiveConfirm = useDestructiveConfirm();

  const mutate = useCallback(
    async (
      args: DropRemoteSchemaPermissionsArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      destructiveConfirm({
        resourceName: args.role,
        resourceType: 'remote schema permission',
        destroyTerm: 'remove',
        appendTerm: `This removes the permission set for role "${args.role}".`,
        onConfirm: () =>
          new Promise<boolean>((resolve) => {
            const query = getDropRemoteSchemaPermissionQuery(
              args.role,
              args.remoteSchemaName,
            );

            mutation.mutate(
              {
                query,
              },
              {
                onError: (error) => {
                  showErrorNotification({
                    title: 'Removing permission failed',
                    error,
                  });
                  onError?.(error);
                  resolve(false);
                },
                onSuccess: () => {
                  hasuraToast({
                    type: 'success',
                    title: 'Success!',
                    message: 'Permission removed successfully',
                  });

                  if (onSuccess) onSuccess();
                  resolve(true);
                },
              },
            );
          }),
      });
    },
    [mutation, showErrorNotification, destructiveConfirm],
  );

  return {
    ...mutation,
    mutate,
  };
};
