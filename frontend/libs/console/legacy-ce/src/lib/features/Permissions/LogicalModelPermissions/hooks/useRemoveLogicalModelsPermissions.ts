import { useCallback } from 'react';
import { useMetadataMigration, useMetadata } from '@hasura/metadata/api';
import { LogicalModel, Source } from '@hasura/shared/types';
import { Permission } from '../components/types';
import { errorTransform } from './utils/errorTransform';
import { getDeleteLogicalModelBody } from './utils/getDeleteLogicalModelBody';
import { hasuraToast } from '@hasura/shared/ui';

const useRemoveLogicalModelsPermissions = ({
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
      if (!source) return;

      const { data, error } = await refetchMetadata();
      if (!data) {
        throw error || new Error('failed to fetch metadata');
      }

      const body = getDeleteLogicalModelBody({
        permission,
        logicalModelName,
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
                message: 'Permissions successfully deleted!',
              });
            },
            onError: (err) => {
              hasuraToast({
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
    [mutate, source],
  );

  return {
    remove,
    ...mutate,
  };
};

export { useRemoveLogicalModelsPermissions };
