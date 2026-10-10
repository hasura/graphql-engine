import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useInvalidateMetadata } from '../metadata/useInvalidateMetadata';
import { hasuraToast } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';

export const useReloadRemoteSchema = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const invalidate = useInvalidateMetadata();

  return (remoteSchemaName: string) => {
    return fetchJson(endpoints.metadata, {
      method: 'POST',
      body: JSON.stringify({
        type: 'reload_remote_schema',
        args: {
          name: remoteSchemaName,
        },
      }),
    })
      .then(() => {
        hasuraToast({
          type: 'success',
          title: 'Remote schema cache reloaded',
          message: `Remote schema cache for ${name} has been reloaded`,
        });
      })
      .catch((e) => {
        hasuraToast({
          type: 'error',
          title: 'Error reloading remote schema cache',
          message: `Error reloading remote schema cache for ${name}: ${e.message}`,
        });
      })
      .finally(() => {
        invalidate({
          componentName: 'useReloadRemoteSchema',
        });
      });
  };
};
