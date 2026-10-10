import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableIndex } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useTableIndexes = <T = TableIndex[]>(
  { table, source }: { table: Table; source: QualifiedDataSource },
  options?: Omit<
    UseQueryOptions<TableIndex[], unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source.name, table, 'GET_TABLE_INDEXES'],
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getTableIndexes) return [];
      return dataSource.introspection.getTableIndexes({
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
