import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';

import { useFireNotification } from '@/new-components/Notifications';
import produce from 'immer';

import type { TrackableTable } from '../types';
import { useMetadataSource } from './useMetadataSource';

export const useUntrackTable = (dataSourceName: string) => {
  const mutation = useMetadataMigration();
  const queryClient = useQueryClient();

  const { fireNotification } = useFireNotification();
  const { data } = useMetadataSource(dataSourceName);

  const untrackTable = useCallback(
    async (table: TrackableTable) => {
      if (!data?.driver)
        throw Error('useTrackTable: cannot find source in metadata');

      mutation.mutate(
        {
          query: {
            type: `${data.driver}_untrack_table` as allowedMetadataTypes,
            args: {
              source: dataSourceName,
              table: table.name,
            },
            resource_version: data.metadata.resource_version,
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
                    draft[idx] = { ...table, is_tracked: false };
                  }
                });
                return newTables;
              }
            );

            fireNotification({
              title: 'Success',
              message: 'Table was untracked',
              type: 'success',
            });
          },
          onError: err => {
            fireNotification({
              title: 'Error',
              message: err.message,
              type: 'error',
            });
          },
          onSettled() {
            queryClient.invalidateQueries('treeview');
            queryClient.invalidateQueries([
              'introspected-tables',
              dataSourceName,
            ]);
          },
        }
      );
    },
    [mutation, fireNotification, queryClient, data, dataSourceName]
  );

  return { untrackTable };
};
