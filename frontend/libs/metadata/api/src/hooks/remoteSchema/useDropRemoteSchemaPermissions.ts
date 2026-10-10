import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
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

  const mutate = useCallback(
    async (
      args: DropRemoteSchemaPermissionsArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const isOk = getConfirmation(
        'This will remove the permission for this role',
      );
      if (!isOk) return;

      const query = getDropRemoteSchemaPermissionQuery(
        args.role,
        args.remoteSchemaName,
      );

      return mutation.mutate(
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
