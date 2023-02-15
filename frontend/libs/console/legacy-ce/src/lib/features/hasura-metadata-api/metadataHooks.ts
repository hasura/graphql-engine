import { useQuery } from 'react-query';
import { exportMetadata } from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { DEFAULT_STALE_TIME } from '../DatabaseRelationships';
import { areTablesEqual } from './areTablesEqual';

export const useMetadata = () => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['export_metadata'],
    queryFn: async () => {
      const result = await exportMetadata({ httpClient });
      return result;
    },
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};

export const useResourceVersion = () => {
  const { data: result, isFetching } = useMetadata();
  return useQuery({
    queryKey: ['export_metadata', 'resource_version'],
    queryFn: async () => {
      if (!result) throw Error('Unable to find metadata');

      const { resource_version } = result;
      return resource_version;
    },
    refetchOnWindowFocus: false,
    enabled: !isFetching,
  });
};

export const useMetadataSource = (dataSourceName: string) => {
  const { data: result, isFetching } = useMetadata();
  return useQuery({
    queryKey: ['export_metadata', dataSourceName],
    queryFn: async () => {
      const metadataSource = result?.metadata.sources.find(
        s => s.name === dataSourceName
      );
      return metadataSource;
    },
    enabled: !isFetching,
    refetchOnWindowFocus: false,
  });
};

export const useMetadataTable = (dataSourceName: string, table: Table) => {
  const { data: metadataSource, isFetching } =
    useMetadataSource(dataSourceName);
  return useQuery({
    queryKey: ['export_metadata', dataSourceName, table],
    queryFn: async () => {
      const metadataTable = metadataSource?.tables.find(t =>
        areTablesEqual(t.table, table)
      );
      return metadataTable;
    },
    enabled: !isFetching,
    refetchOnWindowFocus: false,
  });
};

export const useMetadataTables = (dataSourceName: string) => {
  const { data: metadataSource, isFetching } =
    useMetadataSource(dataSourceName);
  return useQuery({
    queryKey: ['export_metadata', dataSourceName, 'tables'],
    queryFn: async () => {
      const metadataTables = metadataSource?.tables;
      return metadataTables;
    },
    enabled: !isFetching && !!dataSourceName,
    refetchOnWindowFocus: false,
  });
};

export const useMetadataSources = () => {
  const { data: metadata, isFetching } = useMetadata();
  return useQuery({
    queryKey: ['export_metadata', 'sources'],
    queryFn: async () => {
      const metadataSources = metadata?.metadata.sources;
      return metadataSources;
    },
    enabled: !isFetching,
    refetchOnWindowFocus: false,
  });
};
