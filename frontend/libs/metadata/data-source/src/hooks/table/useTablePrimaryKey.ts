import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableKeyConstraint } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useTablePrimaryKey = <T = TableKeyConstraint | null>(
  { table, source }: { table: Table; source: QualifiedDataSource },
  options?: Omit<
    UseQueryOptions<TableKeyConstraint | null, unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source.name, table, 'GET_TABLE_PRIMARY_KEY'],
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getPrimaryKey) return null;
      return dataSource.introspection.getPrimaryKey({
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
