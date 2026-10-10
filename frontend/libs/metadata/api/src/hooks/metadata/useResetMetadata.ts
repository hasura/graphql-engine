import { useAuthFetchJson } from '@hasura/shared/hooks';
import { hasuraToast } from '@hasura/shared/ui';
import { createTextWithLinks } from '../../utils/hyperlinkErrorMessageLink';
import { useInvalidateMetadata } from './useInvalidateMetadata';
import { useAppContext } from '@hasura/shared/context';
import { useErrorNotification } from '../notification';

const resetMetadataQuery = JSON.stringify({
  type: 'clear_metadata',
  args: {},
});

export const useResetMetadata = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const invalidate = useInvalidateMetadata();
  const showErrorNotification = useErrorNotification();

  return () => {
    return fetchJson<{ warnings: { code: string; message: string }[] }>(
      endpoints.metadata,
      {
        method: 'POST',
        body: resetMetadataQuery,
      },
    )
      .then((response) => {
        response?.warnings?.forEach((i) => {
          hasuraToast({
            type: 'warning',
            title: 'Manual Event Trigger Cleanup Needed',
            children: createTextWithLinks(i.message),
            toastOptions: {
              duration: Infinity,
            },
          });
        });

        hasuraToast({ title: 'Metadata reset successfully!', type: 'success' });
      })
      .catch((error) => {
        showErrorNotification({
          title: 'Metadata reset failed',
          error,
        });
      })
      .finally(() => {
        invalidate({
          componentName: 'useResetMetadata',
        });
      });
  };
};
