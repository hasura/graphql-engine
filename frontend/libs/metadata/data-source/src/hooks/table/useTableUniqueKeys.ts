import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableKeyConstraint } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useTableUniqueKeys = <T = TableKeyConstraint[]>(
  { table, source }: { table: Table; source: QualifiedDataSource },
  options?: Omit<
    UseQueryOptions<TableKeyConstraint[], unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source.name, table, 'GET_TABLE_UNIQUE_KEYS'],
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getUniqueKeys) return [];
      return dataSource.introspection.getUniqueKeys({
        endpoints,
        fetchJson,
        table,
        dataSourceName: source.name,
      });
    },
    ...defaultQueryOptions,
    ...options,
  });
};
