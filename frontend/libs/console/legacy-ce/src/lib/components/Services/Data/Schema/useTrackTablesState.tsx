import React from 'react';
import {
  adaptTrackedTables,
  adaptUntrackedTables,
  selectTrackedTables,
} from '../../../../features/Data/ManageTable/selectors';
import { useIntrospectedTables } from '../../../../features/Data/hooks/useIntrospectedTables';
import { Feature, IntrospectedTable } from '../../../../features/DataSource';
import { useMetadata } from '../../../../features/hasura-metadata-api';

export function useTrackTablesState(dataSourceName: string, schema: string) {
  const [tab, setTab] = React.useState<'tracked' | 'untracked'>('untracked');

  const {
    data: metadataTables = [],
    isFetched,
    isLoading: isMetaLoading,
    isError: isMetadataError,
    error: metadataError,
  } = useMetadata(m => selectTrackedTables(m)(dataSourceName));

  // if this is not memoized it re-runs each render
  const select = React.useCallback(
    (introspectedTables: Feature | IntrospectedTable[]) => {
      const untracked =
        adaptUntrackedTables(metadataTables)(introspectedTables);
      const untrackedBySchema = untracked.filter(
        t => t.name.split('.')[0] === schema
      );
      const tracked = adaptTrackedTables(metadataTables)(introspectedTables);
      const trackedBySchema = tracked.filter(
        t => t.name.split('.')[0] === schema
      );
      return {
        untrackedTables: untrackedBySchema,
        trackedTables: trackedBySchema,
      };
    },
    [metadataTables, schema]
  );

  const {
    data: { untrackedTables = [], trackedTables = [] } = {},
    isLoading: isIntroLoading,
    isError: isIntrospectionError,
    error: introspectionError,
  } = useIntrospectedTables({
    dataSourceName,
    options: {
      select,
      enabled: isFetched,
      refetchOnWindowFocus: false,
      onSuccess: data => {
        if (data?.untrackedTables.length === 0) {
          // if user has no tracked tables, switch to the untracked list
          setTab('tracked');
        }
      },
    },
  });

  return {
    untrackedTables,
    trackedTables,
    isMetaLoading,
    isMetadataError,
    metadataError,
    isIntroLoading,
    isIntrospectionError,
    introspectionError,
    tab,
    setTab,
  };
}
