import { DataSource, Feature } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
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
    queryKey: [table, dataSourceName],
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
  });
};
