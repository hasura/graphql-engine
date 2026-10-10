import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import type { RemoteSchema } from '@hasura/shared/types';
import { useErrorNotification } from '../notification';

export const useAddRemoteSchema = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      args: RemoteSchema,
      onSuccess?: (name: string) => void,
      onError?: (err: unknown) => void,
    ) => {
      return mutation.mutate(
        {
          query: { type: 'add_remote_schema', args },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Creating remote schema failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Remote Schema created successfully',
            });

            if (onSuccess) onSuccess(args.name);
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
