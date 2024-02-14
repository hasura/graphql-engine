import { DEFAULT_STALE_TIME } from '../../DatabaseRelationships';
import { DataSource, OrderBy, WhereClause } from '../../DataSource';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';
import { AxiosInstance } from 'axios';
import { useQuery } from 'react-query';
import { isScalarGraphQLType } from '../BrowseRows.utils';

type FetchRowsArgs = {
  columns: UseRowsPropType['columns'];
  dataSourceName: UseRowsPropType['dataSourceName'];
  httpClient: AxiosInstance;
  options: UseRowsPropType['options'];
  table: UseRowsPropType['table'];
};

export const fetchRows = async ({
  columns: columnsProp,
  dataSourceName,
  httpClient,
  options,
  table,
}: FetchRowsArgs) => {
  const tableColumns = await DataSource(httpClient).getTableColumns({
    dataSourceName,
    table,
  });

  const columns =
    columnsProp ??
    tableColumns
      // Filter out columns that are objects or arrays
      // We do this because generateGraphQLSelectQuery cannot handle those types
      // TODO: Remove this filter once we improve generateGraphQLSelectQuery
      .filter(column => {
        if (typeof column.graphQLProperties?.graphQLType !== 'undefined') {
          return isScalarGraphQLType(column);
        }
        return true;
      })
      .map(column => column.name);

  const result = await DataSource(httpClient).getTableRows({
    dataSourceName,
    table,
    columns,
    options,
  });

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

export function getBrowseRowsQueryKey({
  dataSourceName,
  table,
  columns,
  options,
}: UseRowsPropType) {
  return [
    'browse-rows',
    dataSourceName,
    table,
    columns,
    JSON.stringify(options),
  ];
}

export const useRows = ({
  dataSourceName,
  table,
  columns,
  options,
}: UseRowsPropType) => {
  const httpClient = useHttpClient();
  const queryKey = getBrowseRowsQueryKey({
    dataSourceName,
    table,
    columns,
    options,
  });

  return useQuery({
    queryKey,
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
