import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import produce from 'immer';
import type { TrackableTable } from '../types';

export const useTrackTable = (dataSourceName: string) => {
  const mutation = useMetadataMigration();

  const { fireNotification } = useFireNotification();
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();
  // const { data } = useMetadataSource(dataSourceName);

  const trackTable = useCallback(
    async (table: TrackableTable) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });
      const currentMetadataSource = metadata.sources?.find(
        source => source.name === dataSourceName
      );
      if (!currentMetadataSource)
        throw Error(`useTrackTable.currentMetadataSource not found`);

      const driver = currentMetadataSource.kind;

      if (!driver) throw Error('useTrackTable: cannot find source in metadata');

      mutation.mutate(
        {
          query: {
            type: `${driver}_track_table` as allowedMetadataTypes,
            args: {
              source: dataSourceName,
              table: table.name,
            },
            resource_version,
          },
        },
        {
          onSuccess: () => {
            // update cache
            queryClient.setQueriesData<TrackableTable[] | undefined>(
              [dataSourceName, 'tables'],
              oldTables => {
                const newTables = produce(oldTables, draft => {
                  const idx = draft?.findIndex(ta => ta.name === table.name);
                  if (draft && idx !== undefined && idx !== -1) {
                    draft[idx] = { ...table, is_tracked: true };
                  }
                });
                return newTables;
              }
            );

            fireNotification({
              title: 'Success',
              message: 'Table was tracked',
              type: 'success',
            });
          },
          onError: () => {
            fireNotification({
              title: 'Error',
              message: 'Table could not be tracked',
              type: 'error',
            });
          },
          onSettled() {
            queryClient.invalidateQueries('treeview');
            queryClient.refetchQueries(
              ['introspected-tables', dataSourceName],
              { exact: true }
            );
            queryClient.refetchQueries(['export_metadata'], { exact: true });
          },
        }
      );
    },
    [httpClient, mutation, dataSourceName, queryClient, fireNotification]
  );

  return { trackTable };
};
