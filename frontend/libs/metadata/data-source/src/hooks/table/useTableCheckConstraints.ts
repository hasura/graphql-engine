import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableCheckConstraint } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getTableCheckConstraintsQueryKey } from '../../types/queryKey';

export type UseTableCheckConstraintsProps = {
  table: Table;
  source: QualifiedDataSource;
};

export const useTableCheckConstraints = <T = TableCheckConstraint[]>(
  { table, source }: UseTableCheckConstraintsProps,
  options?: Omit<
    UseQueryOptions<TableCheckConstraint[], unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getTableCheckConstraintsQueryKey(source.name, table),
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getCheckConstraints) {
        return [];
      }

      return dataSource.introspection.getCheckConstraints({
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
