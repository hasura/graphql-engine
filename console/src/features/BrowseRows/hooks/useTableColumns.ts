import { DEFAULT_STALE_TIME } from '@/features/DatabaseRelationships';
import { DataSource, Feature } from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';

export const useTableColumns = ({
  table,
  dataSourceName,
}: {
  table: Table;
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['column-introspection', dataSourceName, table],
    queryFn: async () => {
      const columns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      const supportedOperators = await DataSource(
        httpClient
      ).getSupportedOperators({
        dataSourceName,
      });

      return {
        columns,
        supportedOperators:
          supportedOperators === Feature.NotImplemented
            ? []
            : supportedOperators,
      };
    },
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};
