import { useCallback } from 'react';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigration } from '@hasura/metadata/api';

export const useReloadSource = () => {
  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({ type: 'success', title: 'Reload successful!' });
    },
    onError: (error) => {
      showErrorNotification({ error, title: 'Failed to reload source.' });
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
    [mutate],
  );
  return { reloadSource, ...rest };
};
