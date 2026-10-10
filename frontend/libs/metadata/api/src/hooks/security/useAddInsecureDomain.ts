import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { getErrorMessage } from '@hasura/shared/utils';
import { useMetadataMigration } from '../metadata';

export const useAddInsecureDomain = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (host: string, port?: string) => {
      return mutation.mutate(
        {
          query: {
            type: 'add_host_to_tls_allowlist',
            args: { host, permissions: ['self-signed'], suffix: port },
          },
        },
        {
          onSuccess: async () => {
            hasuraToast({
              title: 'Domain added!',
              message: 'Domain added to insecure TLS allow list successfully',
              type: 'success',
            });

            return true;
          },
          onError: async (err) => {
            hasuraToast({
              title: 'Adding domain failed!',
              message: getErrorMessage(err),
              type: 'error',
            });

            return false;
          },
        },
      );
    },
    [mutation],
  );
};

export default useAddInsecureDomain;
