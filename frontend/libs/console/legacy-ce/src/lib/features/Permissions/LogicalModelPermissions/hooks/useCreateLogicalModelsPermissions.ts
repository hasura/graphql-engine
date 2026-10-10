import { useCallback } from 'react';
import { useMetadataMigration, useMetadata } from '@hasura/metadata/api';
import { LogicalModel, Source } from '@hasura/shared/types';
import { errorTransform } from './utils/errorTransform';
import { getCreateLogicalModelBody } from './utils/getCreateLogicalModelBody';
import { hasuraToast } from '@hasura/shared/ui';

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
  const { refetch: refetchMetadata } = useMetadata(undefined, {
    enabled: false,
  });

  const create = useCallback(
    async ({ permission, logicalModelName, onSuccess }) => {
      if (!source) return;
      const { data, error } = await refetchMetadata();
      if (!data) {
        throw error || new Error('failed to fetch metadata');
      }

      const body = getCreateLogicalModelBody({
        permission,
        logicalModelName,
        logicalModels,
        source,
      });

      try {
        await mutate.mutate(
          {
            query: {
              type: 'bulk',
              args: body,
              resource_version: data.resource_version,
            },
          },
          {
            onSuccess: async () => {
              hasuraToast({
                type: 'success',
                title: 'Success!',
                message: 'Permissions saved successfully!',
              });
            },
            onError: (err) => {
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
          },
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
    [logicalModels, source],
  );

  return {
    create,
    ...mutate,
  };
};

export { useCreateLogicalModelsPermissions };
