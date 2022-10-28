import { exportMetadata } from '@/features/DataSource';
import { Source } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { AxiosError } from 'axios';
import isEqual from 'lodash.isequal';
import React from 'react';
import { useQuery } from 'react-query';

export const getTableFromMetadata = (
  metadata: Source | undefined,
  table: unknown
) => {
  if (!table) return null;
  return metadata?.tables.find(t => isEqual(t.table, table));
};

export const manageTableMetadataQueryKey = (dataSourceName: string) => [
  'manage-table-metadata',
  dataSourceName,
];

// adding underscore to prevent conflicts or confusion as this is only mean to be for this scope
export const useMetadataForManageTable = (dataSourceName: string) => {
  const httpClient = useHttpClient();

  return useQuery<
    { metadata: Source; resource_version: number } | undefined,
    AxiosError
  >({
    queryKey: manageTableMetadataQueryKey(dataSourceName),
    queryFn: async () => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Unable to fetch sources from metadata');

      const metadataSource = metadata.sources.find(
        source => source.name === dataSourceName
      );

      if (!metadataSource) throw Error('Unable to find the source in metadata');

      const data = {
        metadata: metadataSource,
        resource_version,
      };

      console.log('DATA', data);

      return data;
    },

    refetchOnWindowFocus: false,
  });
};

export const useMetadataTable = (dataSourceName: string, table: unknown) => {
  const { data, isLoading } = useMetadataForManageTable(dataSourceName);

  const metadataTable = React.useMemo(
    () => getTableFromMetadata(data?.metadata, table),
    [table, data?.metadata]
  );

  return {
    metadata: data?.metadata,
    isLoading,
    metadataTable,
  };
};
