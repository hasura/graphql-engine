import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import {
  getSourceFromInconsistentObjects,
  useMetadataMigrationMutation,
} from '@hasura/metadata/api';
import type { InconsistentObject } from '@hasura/shared/types';

type ReloadMetadataResponse = {
  is_consistent?: boolean;
  inconsistent_objects?: InconsistentObject[];
};

/**
 * reload_metadata succeeds even when the source cannot be reached: the source
 * (or any of its objects, e.g. a dropped table) is then reported in the
 * inconsistent objects of the response. HGE only includes
 * `inconsistent_objects` when the metadata is inconsistent.
 */
export const isSourceInconsistentAfterReload = (
  response: ReloadMetadataResponse | undefined,
  dataSourceName: string,
) =>
  getSourceFromInconsistentObjects(
    response?.inconsistent_objects ?? [],
  ).includes(dataSourceName);

export const useReloadSource = () => {
  return useMetadataMigrationMutation<ReloadMetadataResponse, string>(
    async (dataSourceName) => ({
      type: 'reload_metadata',
      args: { reload_sources: [dataSourceName] },
    }),
    {
      onSuccess: (data, dataSourceName) => {
        if (isSourceInconsistentAfterReload(data, dataSourceName)) {
          hasuraToast({
            type: 'error',
            title: 'Reloaded with inconsistencies',
            message: `"${dataSourceName}" or some of its objects are inconsistent. Check Settings > Metadata Status for details.`,
          });
          return;
        }

        hasuraToast({ type: 'success', title: 'Reload successful!' });
      },
      onError: (error) => {
        showErrorNotification({ error, title: 'Failed to reload source.' });
      },
    },
  );
};
