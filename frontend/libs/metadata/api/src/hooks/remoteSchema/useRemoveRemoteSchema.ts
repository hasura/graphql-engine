import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export const useRemoveRemoteSchema = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      name: string,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      return mutation.mutate(
        {
          query: {
            type: 'remove_remote_schema',
            args: { name },
          },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Delete remote schema failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Remote Schema deleted successfully',
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
