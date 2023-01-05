import { DEFAULT_STALE_TIME } from '@/features/DatabaseRelationships';
import { DataSource, OrderBy, WhereClause } from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { AxiosInstance } from 'axios';
import { useQuery } from 'react-query';

type FetchRowsArgs = {
  columns: UseRowsPropType['columns'];
  dataSourceName: UseRowsPropType['dataSourceName'];
  httpClient: AxiosInstance;
  options: UseRowsPropType['options'];
  table: UseRowsPropType['table'];
};

export const fetchRows = async ({
  columns,
  dataSourceName,
  httpClient,
  options,
  table,
}: FetchRowsArgs) => {
  const tableColumns = await DataSource(httpClient).getTableColumns({
    dataSourceName,
    table,
  });

  console.log('>>>', columns, tableColumns);

  const result = await DataSource(httpClient).getTableRows({
    dataSourceName,
    table,
    columns: columns ?? tableColumns.map(column => column.name),
    options,
  });

  console.log('>>>', result);

  return result;
};

export type UseRowsPropType = {
  dataSourceName: string;
  table: Table;
  columns?: string[];
  options?: {
    where?: WhereClause[];
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
    queryKey: [
      'browse-rows',
      dataSourceName,
      table,
      columns,
      JSON.stringify(options),
    ],
    queryFn: async () => {
      try {
        return await fetchRows({
          columns,
          dataSourceName,
          httpClient,
          options,
          table,
        });
      } catch (err: any) {
        throw new Error(err);
      }
    },
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};
