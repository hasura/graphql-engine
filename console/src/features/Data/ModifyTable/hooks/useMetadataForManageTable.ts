import { exportMetadata } from '@/features/DataSource';
import { Source } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { AxiosError } from 'axios';
import isEqual from 'lodash.isequal';
import React from 'react';
import { useQuery } from 'react-query';
// eslint-disable-next-line no-restricted-imports
import { useTableColumns } from '@/features/BrowseRows/components/DataGrid/useTableColumns';

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
export const useMetadataForManageTable = (
  dataSourceName: string,
  queryEnabled = true
) => {
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

      return data;
    },
    enabled: queryEnabled,
    refetchOnWindowFocus: false,
  });
};

export const useMetadataTable = (dataSourceName: string, table: unknown) => {
  const { data, ...rest } = useMetadataForManageTable(dataSourceName);

  const metadataTable = React.useMemo(
    () => getTableFromMetadata(data?.metadata, table),
    [table, data?.metadata]
  );

  return {
    metadata: data?.metadata,
    resource_version: data?.resource_version,
    metadataTable,
    ...rest,
  };
};

export const useListAllTableColumns = (
  dataSourceName: string,
  table: unknown
) => {
  const { data: tableColumns, isFetching: isIntrospectionReady } =
    useTableColumns({
      table,
      dataSourceName,
    });

  const { data, ...rest } = useMetadataForManageTable(
    dataSourceName,
    !isIntrospectionReady
  );

  const metadataTable = React.useMemo(
    () => getTableFromMetadata(data?.metadata, table),
    [table, data?.metadata]
  );

  const tableConfig = metadataTable?.configuration?.column_config;

  return {
    data: (tableColumns?.columns ?? []).map(tableColumn => ({
      ...tableColumn,
      config: tableConfig?.[tableColumn.name],
    })),
    ...rest,
  };
};
