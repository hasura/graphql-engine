import { useMetadataMigration } from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import React, { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { useIsUnmounted } from '@/components/Services/Data';
import { useMetadataSource, tablesQueryKey } from '@/features/Data';
import type { TrackableTable } from '../types';
import { buildTrackingQuery, TrackingQuery } from './buildTrackingQuery';

export const useTrackTable = (dataSourceName: string) => {
  const mutation = useMetadataMigration();

  const [loading, setLoading] = React.useState(false);

  const { fireNotification } = useFireNotification();

  const unMounted = useIsUnmounted();

  const { data } = useMetadataSource(dataSourceName);

  const metadata = data?.metadata;

  const queryClient = useQueryClient();

  // wrapping in promise so outside consumers can have simpler api
  const doMutation = (query: TrackingQuery) =>
    new Promise((res, rej) => {
      mutation.mutate(
        {
          query,
        },
        {
          onSuccess: res,
          onError: err => rej(err.message),
        }
      );
    });

  const changeTableTracking = useCallback(
    (action: 'track' | 'untrack', tables: TrackableTable[]) => {
      if (!metadata) throw Error('useTrackTable: cannot export metadata');

      const driver = metadata.metadata.sources.find(
        source => source.name === dataSourceName
      );
      if (!metadata) throw Error(`useTrackTable metadata not found`);

      if (!driver) throw Error('useTrackTable: cannot find source in metadata');

      const noun = tables.length > 1 ? 'Tables' : tables[0].name;
      const verb = tables.length > 1 ? 'were' : 'was';

      const query = buildTrackingQuery(
        dataSourceName,
        metadata.resource_version,
        driver.kind,
        action,
        tables
      );

      // if more than 500ms elapse before promise resolves, then set the loading state to true
      const loadingTimer = setTimeout(() => {
        setLoading(true);
      }, 250);

      return doMutation(query)
        .then(() => {
          // all these return promises so there's no chance of unintentionally getting thrown into the catch block
          queryClient.invalidateQueries(tablesQueryKey(dataSourceName));
          queryClient.invalidateQueries('treeview');
          queryClient.invalidateQueries(['trackTables', 'metadataSource']);

          fireNotification({
            title: 'Success',
            message: `${noun} ${verb} ${action}ed`,
            type: 'success',
          });
        })
        .catch((message: string) => {
          fireNotification({
            title: 'Error',
            message: message || `${noun} could not be ${action}ed`,
            type: 'error',
          });
        })
        .finally(() => {
          clearTimeout(loadingTimer);
          if (!unMounted) setLoading(false);
        });
    },
    [metadata, mutation, queryClient, fireNotification, data]
  );

  // track single
  const trackTable = useCallback(
    (table: TrackableTable) => changeTableTracking('track', [table]),
    [changeTableTracking]
  );

  // untrack single
  const untrackTable = useCallback(
    (table: TrackableTable) => changeTableTracking('untrack', [table]),
    [changeTableTracking]
  );

  // track multiple
  const trackTables = useCallback(
    (tables: TrackableTable[]) => changeTableTracking('track', tables),
    [changeTableTracking]
  );

  // untrack multiple
  const untrackTables = useCallback(
    (tables: TrackableTable[]) => changeTableTracking('untrack', tables),
    [changeTableTracking]
  );

  return { loading, trackTable, untrackTable, trackTables, untrackTables };
};
