import { hasuraToast } from '@hasura/shared/ui';
import { createTextWithLinks } from '../../utils/hyperlinkErrorMessageLink';
import { useInvalidateMetadata } from './useInvalidateMetadata';
import { useErrorNotification } from '../notification';
import { useMetadataMigrationMutation } from './useMetadataMigration';

const resetMetadataQuery = {
  type: 'clear_metadata' as const,
  args: {},
};

export const useResetMetadata = () => {
  const invalidate = useInvalidateMetadata();
  const showErrorNotification = useErrorNotification();

  return useMetadataMigrationMutation(async () => resetMetadataQuery, {
    onSuccess: (response: {
      warnings?: { code: string; message: string }[];
    }) => {
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
    },
    onError: (error) => {
      // Success already invalidates via `postMetadata`. A failed reset may still
      // have cleared part of the metadata, so refresh the cache here too.
      invalidate({
        componentName: 'useResetMetadata',
      });

      showErrorNotification({
        title: 'Metadata reset failed',
        error,
      });
    },
  });
};
