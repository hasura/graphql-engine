import { useCallback } from 'react';
import { hasuraToast } from '../../../new-components/Toasts';
import { useMetadataMigration } from '../../MetadataAPI';

export const useReloadSource = () => {
  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({ type: 'success', title: 'Reload successful!' });
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
