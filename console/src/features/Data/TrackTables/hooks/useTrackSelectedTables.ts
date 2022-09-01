import React from 'react';
import { useQueryClient } from 'react-query';
import {
  allowedMetadataTypes,
  useMetadata,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';

import type { TrackableTable } from '../types';

export const useTrackSelectedTables = () => {
  const mutation = useMetadataMigration();

  const { fireNotification } = useFireNotification();
  const { data: metadata } = useMetadata();

  const queryClient = useQueryClient();

  const trackOrUntrackSelectedTables = React.useCallback(
    (
      track: 'track' | 'untrack',
      dataSourceName: string,
      tables: TrackableTable[]
    ) => {
      if (!metadata) throw Error('useTrackAllTables: cannot export metadata');

      const driver = metadata.metadata.sources.find(
        source => source.name === dataSourceName
      )?.kind;

      const type =
        track === 'track'
          ? (`${driver}_track_table` as allowedMetadataTypes)
          : (`${driver}_untrack_table` as allowedMetadataTypes);

      const args = tables.map((table: TrackableTable) => {
        return {
          type,
          args: {
            source: dataSourceName,
            table: table.table,
          },
        };
      });

      const query = {
        type: 'bulk' as allowedMetadataTypes,
        source: 'default',
        resource_version: metadata.resource_version,
        args,
      };

      mutation.mutate(
        {
          query,
        },
        {
          onSuccess: () => {
            queryClient.invalidateQueries([dataSourceName, 'tables']);

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
    [metadata, mutation, queryClient, fireNotification]
  );

  const trackSelectedTables = (
    dataSourceName: string,
    tables: TrackableTable[]
  ) => {
    const filteredTables = tables.filter(
      ({ is_tracked }) => is_tracked === false
    );

    trackOrUntrackSelectedTables('track', dataSourceName, filteredTables);
  };

  const unTrackSelectedTables = (
    dataSourceName: string,
    tables: TrackableTable[]
  ) => {
    const filteredTables = tables.filter(
      ({ is_tracked }) => is_tracked === true
    );

    trackOrUntrackSelectedTables('untrack', dataSourceName, filteredTables);
  };

  return { trackSelectedTables, unTrackSelectedTables };
};
