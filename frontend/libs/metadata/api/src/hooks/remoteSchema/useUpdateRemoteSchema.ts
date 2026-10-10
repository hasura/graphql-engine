import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import type { RemoteSchema } from '@hasura/shared/types';
import { useErrorNotification } from '../notification';

export const useUpdateRemoteSchema = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      args: RemoteSchema,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      return mutation.mutate(
        {
          query: { type: 'update_remote_schema', args },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Updating remote schema failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Remote Schema updated successfully',
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
