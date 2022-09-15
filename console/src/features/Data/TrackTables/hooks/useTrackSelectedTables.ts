import React from 'react';
import { useQueryClient } from 'react-query';
import { useMetadataMigration } from '@/features/MetadataAPI';

import { useFireNotification } from '@/new-components/Notifications';

import type { TrackableTable } from '../types';
import { useMetadataSource } from './useMetadataSource';

export const useTrackSelectedTables = (dataSourceName: string) => {
  const mutation = useMetadataMigration();

  const { fireNotification } = useFireNotification();

  const { data } = useMetadataSource(dataSourceName);
  const queryClient = useQueryClient();

  const trackOrUntrackSelectedTables = React.useCallback(
    async (track: 'track' | 'untrack', tables: TrackableTable[]) => {
      if (!data?.metadata)
        throw Error('useTrackAllTables: cannot export metadata');

      const driver = data.metadata.metadata.sources.find(
        source => source.name === dataSourceName
      )?.kind;

      const type =
        track === 'track' ? `${driver}_track_table` : `${driver}_untrack_table`;

      const args = tables.map((trackableTable: TrackableTable) => {
        return {
          type,
          args: {
            source: dataSourceName,
            table: trackableTable.table,
          },
        };
      });

      const query = {
        type: 'bulk' as const,
        source: 'default',
        resource_version: data.metadata.resource_version,
        args,
      };

      mutation.mutate(
        {
          query,
        },
        {
          onSuccess: () => {
            queryClient.invalidateQueries([dataSourceName, 'tables']);
            queryClient.invalidateQueries('treeview');
            queryClient.invalidateQueries(['trackTables', 'metadataSource']);

            fireNotification({
              title: 'Success',
              message: `Tables were ${track}ed`,
              type: 'success',
            });
          },
          onError: () => {
            fireNotification({
              title: 'Error',
              message: `Tables could not be ${track}ed`,
              type: 'error',
            });
          },
        }
      );
    },
    [mutation, queryClient, fireNotification, data, dataSourceName]
  );

  const trackSelectedTables = (tables: TrackableTable[]) => {
    const filteredTables = tables.filter(
      ({ is_tracked }) => is_tracked === false
    );

    trackOrUntrackSelectedTables('track', filteredTables);
  };

  const unTrackSelectedTables = (tables: TrackableTable[]) => {
    const filteredTables = tables.filter(
      ({ is_tracked }) => is_tracked === true
    );

    trackOrUntrackSelectedTables('untrack', filteredTables);
  };

  return { trackSelectedTables, unTrackSelectedTables };
};
