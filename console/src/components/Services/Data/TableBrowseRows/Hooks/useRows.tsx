import { DataSource, OrderBy, WhereClause } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';

export type UseRowsPropType = {
  dataSourceName: string;
  table: Table;
  columns?: string[];
  options?: {
    where?: WhereClause;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
};

export const useRows = ({
  dataSourceName,
  table,
  columns,
  options,
}: UseRowsPropType) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['browse-rows', dataSourceName, table, columns],
    queryFn: async () => {
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      const result = await DataSource(httpClient).getTableRows({
        dataSourceName,
        table,
        columns: columns ?? tableColumns.map(column => column.name),
        options,
      });

      return result;
    },
  });
};
