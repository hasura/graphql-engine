import { useCallback } from 'react';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import {
  getSourceFromInconsistentObjects,
  useMetadataMigration,
} from '@hasura/metadata/api';
import type { InconsistentObject } from '@hasura/shared/types';

type ReloadMetadataResponse = {
  is_consistent?: boolean;
  inconsistent_objects?: InconsistentObject[];
};

/**
 * reload_metadata succeeds even when the source cannot be reached: the source
 * is then reported in the inconsistent objects of the response.
 */
export const isSourceInconsistentAfterReload = (
  response: ReloadMetadataResponse | undefined,
  dataSourceName: string,
) =>
  response?.is_consistent === false &&
  getSourceFromInconsistentObjects(
    response.inconsistent_objects ?? [],
  ).includes(dataSourceName);

export const useReloadSource = () => {
  const { mutate, ...rest } = useMetadataMigration<ReloadMetadataResponse>({
    onError: (error) => {
      showErrorNotification({ error, title: 'Failed to reload source.' });
    },
  });

  const reloadSource = useCallback(
    async (dataSourceName: string) => {
      mutate(
        {
          query: {
            type: 'reload_metadata',
            args: { reload_sources: [dataSourceName] },
          },
        },
        {
          onSuccess: (data) => {
            if (isSourceInconsistentAfterReload(data, dataSourceName)) {
              hasuraToast({
                type: 'error',
                title: 'Source is inconsistent',
                message: `"${dataSourceName}" was reloaded but is still inconsistent. Check Settings > Metadata Status for details.`,
              });
              return;
            }
            hasuraToast({ type: 'success', title: 'Reload successful!' });
          },
        },
      );
    },
    [mutate],
  );
  return { reloadSource, ...rest };
};
