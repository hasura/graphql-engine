import { DataSource, Feature } from '../../DataSource';
import { DEFAULT_STALE_TIME } from '../../hasura-metadata-api/useMetadata';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';
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
