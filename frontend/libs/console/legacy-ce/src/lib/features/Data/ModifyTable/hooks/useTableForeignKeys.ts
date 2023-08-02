import { DataSource, Feature } from '../../../DataSource';
import { Table } from '../../../hasura-metadata-types';
import { useHttpClient } from '../../../Network';
import { useQuery } from 'react-query';

export const useTableForeignKeys = ({
  table,
  dataSourceName,
}: {
  table: Table;
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['foreign-keys-introspection', dataSourceName, table],
    queryFn: async () => {
      const foreignKeys = await DataSource(httpClient).getTableFkRelationships({
        dataSourceName,
        table,
      });

      const supportedOperators = await DataSource(
        httpClient
      ).getSupportedOperators({
        dataSourceName,
      });

      return {
        foreignKeys,
        supportedOperators:
          supportedOperators === Feature.NotImplemented
            ? []
            : supportedOperators,
      };
    },
    refetchOnWindowFocus: false,
  });
};
