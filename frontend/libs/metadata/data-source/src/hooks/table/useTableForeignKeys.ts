import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableFkRelationships } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getTableForeignKeysQueryKey } from '../../types/queryKey';

export type UseTableForeignKeysProps = {
  table: Table;
  source: QualifiedDataSource;
};

export const useTableForeignKeys = <T = TableFkRelationships[]>(
  { table, source }: UseTableForeignKeysProps,
  options?: UseQueryOptions<TableFkRelationships[], unknown, T>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getTableForeignKeysQueryKey(source.name, table),
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getFKRelationships) {
        return [];
      }

      return dataSource.introspection.getFKRelationships!({
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
