import { useMetadataMigration } from '../../MetadataAPI';
import { useInvalidateMetadata } from '../../hasura-metadata-api';
import { hasuraToast } from '../../../new-components/Toasts';
import { useCallback } from 'react';

export const useReloadSource = () => {
  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({ type: 'success', title: 'Reload successful!' });
      invalidateMetadata();
    },
    onError: () => {
      hasuraToast({ type: 'error', title: 'Failed to reload source.' });
    },
  });
  const reloadSource = useCallback(
    async (dataSourceName: string) => {
      mutate({
        query: {
          type: 'reload_metadata',
          args: { reload_sources: [dataSourceName] },
        },
      });
    },
    [mutate]
  );
  return { reloadSource, ...rest };
};
