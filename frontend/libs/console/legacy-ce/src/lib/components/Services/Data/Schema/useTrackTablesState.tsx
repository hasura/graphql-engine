import React from 'react';
import {
  selectTrackedTables,
  splitByTracked,
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
    (introspectedTables: Feature | IntrospectedTable[]) =>
      splitByTracked({
        metadataTables,
        introspectedTables,
        schemaFilter: schema,
      }),
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
