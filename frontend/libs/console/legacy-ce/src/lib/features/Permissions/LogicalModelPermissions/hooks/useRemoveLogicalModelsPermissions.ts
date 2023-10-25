import { useCallback } from 'react';
import { useFireNotification } from '../../../../new-components/Notifications/index';
import { exportMetadata } from '../../../DataSource';
import { useMetadataMigration } from '../../../MetadataAPI';
import { useHttpClient } from '../../../Network';
import { LogicalModel, Source } from '../../../hasura-metadata-types';
import { Permission } from '../components/types';
import { errorTransform } from './utils/errorTransform';
import { getDeleteLogicalModelBody } from './utils/getDeleteLogicalModelBody';

const useRemoveLogicalModelsPermissions = ({
  logicalModels,
  source,
}: {
  logicalModels: LogicalModel[];
  source: Source | undefined;
}) => {
  const mutate = useMetadataMigration({
    errorTransform,
  });
  const { fireNotification } = useFireNotification();
  const httpClient = useHttpClient();

  const remove = useCallback(
    async ({
      permission,
      logicalModelName,
      onSuccess,
    }: {
      permission: Permission;
      logicalModelName: string;
      onSuccess?: () => void;
    }) => {
      const { resource_version } = await exportMetadata({
        httpClient,
      });
      if (!source) return;

      const body = getDeleteLogicalModelBody({
        permission,
        logicalModelName,
        source,
      });

      try {
        await mutate.mutateAsync(
          {
            query: { type: 'bulk', args: body, resource_version },
          },
          {
            onSuccess: async () => {
              fireNotification({
                type: 'success',
                title: 'Success!',
                message: 'Permissions successfully deleted!',
              });
            },
            onError: err => {
              fireNotification({
                type: 'error',
                title: 'Error!',
                message:
                  err?.message ??
                  'Something went wrong while deleting permissions',
              });
            },
            onSettled: async () => {
              onSuccess?.();
            },
          }
        );
      } catch (error: any) {
        fireNotification({
          type: 'error',
          title: 'Error!',
          message:
            error?.message ?? 'Something went wrong while saving permissions',
        });
      }
    },
    [source]
  );

  return {
    remove,
    ...mutate,
  };
};

export { useRemoveLogicalModelsPermissions };
