import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getDatabaseMethods, TableColumn } from '../../driver';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getTableColumnInfosQueryKey } from '../../types/queryKey';

export const useGetTableColumnInfos = <FinalResult = TableColumn[]>(
  {
    source,
    table,
  }: {
    source: QualifiedDataSource;
    table: Table;
  },
  options?: UseQueryOptions<TableColumn[], unknown, FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<TableColumn[], unknown, FinalResult>({
    queryKey: getTableColumnInfosQueryKey(source.name),
    queryFn: async () => {
      const databaseMethods = getDatabaseMethods(source.kind);

      return databaseMethods.introspection.getTableColumnInfos({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
        table,
      });
    },
    ...options,
  });
};
