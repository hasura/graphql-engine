import { useCallback } from 'react';
import { exportMetadata } from '../../../DataSource';
import { useMetadataMigration } from '../../../MetadataAPI';
import { useHttpClient } from '../../../Network';
import { LogicalModel, Source } from '../../../hasura-metadata-types';
import { errorTransform } from './utils/errorTransform';
import { getCreateLogicalModelBody } from './utils/getCreateLogicalModelBody';
import { hasuraToast } from '../../../../new-components/Toasts';

const useCreateLogicalModelsPermissions = ({
  logicalModels,
  source,
}: {
  logicalModels: LogicalModel[];
  source: Source | undefined;
}) => {
  const mutate = useMetadataMigration({
    errorTransform,
  });
  const httpClient = useHttpClient();

  const create = useCallback(
    async ({ permission, logicalModelName, onSuccess }) => {
      const { resource_version } = await exportMetadata({
        httpClient,
      });
      if (!source) return;

      const body = getCreateLogicalModelBody({
        permission,
        logicalModelName,
        logicalModels,
        source,
      });

      try {
        await mutate.mutateAsync(
          {
            query: { type: 'bulk', args: body, resource_version },
          },
          {
            onSuccess: async () => {
              hasuraToast({
                type: 'success',
                title: 'Success!',
                message: 'Permissions saved successfully!',
              });
            },
            onError: err => {
              hasuraToast({
                type: 'error',
                title: 'Error!',
                message:
                  err?.message ??
                  'Something went wrong while saving permissions',
              });
            },
            onSettled: async () => {
              onSuccess?.();
            },
          }
        );
      } catch (error: any) {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message:
            error?.message ?? 'Something went wrong while saving permissions',
        });
      }
    },
    [logicalModels, source]
  );

  return {
    create,
    ...mutate,
  };
};

export { useCreateLogicalModelsPermissions };
