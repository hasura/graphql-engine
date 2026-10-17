import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';

export const useDeleteInsecureDomain = () => {
  return useMetadataMigrationMutation(
    async ({ host, port }: { host: string; port?: string }) => ({
      type: 'drop_host_from_tls_allowlist',
      args: { host, suffix: port },
    }),
    {
      onSuccess: () => {
        hasuraToast({
          title: 'Domain deleted!',
          type: 'success',
        });
      },
      onError: (error) => {
        showErrorNotification({
          title: 'Deleting domain failed!',
          error,
        });
      },
    },
  );
};

export default useDeleteInsecureDomain;
