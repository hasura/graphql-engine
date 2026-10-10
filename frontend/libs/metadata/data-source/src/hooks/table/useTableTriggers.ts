import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableTrigger } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useTableTriggers = <T = TableTrigger[]>(
  { table, source }: { table: Table; source: QualifiedDataSource },
  options?: Omit<
    UseQueryOptions<TableTrigger[], unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source.name, table, 'GET_TABLE_TRIGGERS'],
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getTableTriggers) return [];
      return dataSource.introspection.getTableTriggers({
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
