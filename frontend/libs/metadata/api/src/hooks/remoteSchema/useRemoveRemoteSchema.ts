import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export const useRemoveRemoteSchema = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      args: string | { name: string; cascade?: boolean },
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      // Backwards compatible: callers may pass just the remote schema name, or
      // an object opting into `cascade` (drops dependent objects server-side).
      const { name, cascade } =
        typeof args === 'string' ? { name: args, cascade: undefined } : args;

      return mutation.mutate(
        {
          query: {
            type: 'remove_remote_schema',
            // `cascade` is only sent when explicitly requested so the server
            // keeps reporting dependent objects by default.
            args: cascade ? { name, cascade: true } : { name },
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
    [mutation, showErrorNotification],
  );

  return {
    ...mutation,
    mutate,
  };
};
