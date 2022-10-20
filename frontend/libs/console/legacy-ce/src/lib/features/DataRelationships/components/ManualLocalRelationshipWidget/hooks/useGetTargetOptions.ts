import { useHttpClient } from '@/features/Network';
import { areTablesEqual } from '@/features/RelationshipsTable';
import { exportMetadata } from '@/features/DataSource';
import { useQuery } from 'react-query';
import { Table } from '@/features/MetadataAPI';
import { AxiosError } from 'axios';

export const useGetTargetOptions = (
  dataSourceName: string,
  excludeTable?: Table
) => {
  const httpClient = useHttpClient();

  return useQuery<Table[], AxiosError>({
    queryKey: ['local-db-relationships-target-options', dataSourceName],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });

      if (!metadata) throw Error('Unable to fetch sources from metadata');

      const metadataSource = metadata.sources.find(
        (source) => source.name === dataSourceName
      );

      if (!metadataSource) throw Error('Unable to find the source in metadata');

      const tables = metadataSource.tables.map((t) => t.table);
      if (excludeTable)
        return tables.filter((t) => !areTablesEqual(t, excludeTable));

      return tables;
    },
    refetchOnWindowFocus: false,
  });
};
