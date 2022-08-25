import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

import {
  allowedMetadataTypes,
  useMetadata,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import produce from 'immer';

import type { TrackableTable } from '../types';

export const useTrackTable = () => {
  const mutation = useMetadataMigration();

  const { fireNotification } = useFireNotification();
  const { data: metadata } = useMetadata();
  const queryClient = useQueryClient();

  const trackTable = useCallback(
    (dataSourceName: string, table: TrackableTable) => {
      if (!metadata) throw Error('useTrackTable: cannot export metadata');

      const driver = metadata.metadata.sources.find(
        source => source.name === dataSourceName
      )?.kind;

      if (!driver) throw Error('useTrackTable: cannot find source in metadata');

      mutation.mutate(
        {
          query: {
            type: `${driver}_track_table` as allowedMetadataTypes,
            args: {
              source: dataSourceName,
              table: table.table,
            },
            resource_version: metadata.resource_version,
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
        }
      );
    },
    [metadata, mutation, queryClient, fireNotification]
  );

  return { trackTable };
};
