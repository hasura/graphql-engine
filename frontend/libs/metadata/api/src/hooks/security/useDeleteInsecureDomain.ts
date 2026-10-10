import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { getErrorMessage } from '@hasura/shared/utils';
import { useMetadataMigration } from '../metadata';

export const useDeleteInsecureDomain = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (host: string, port?: string) => {
      mutation.mutate(
        {
          query: {
            type: 'drop_host_from_tls_allowlist',
            args: { host, suffix: port },
          },
        },
        {
          onSuccess: () => {
            hasuraToast({
              title: 'Domain deleted!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Deleting domain failed!',
              message: getErrorMessage(err),
              type: 'error',
            });
          },
        },
      );
    },
    [mutation],
  );
};

export default useDeleteInsecureDomain;
