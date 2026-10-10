import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableFkRelationships } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getTablesForeignKeysQueryKey } from '../../types/queryKey';

export type UseTablesForeignKeysProps = {
  tables: Table[];
  source: QualifiedDataSource;
};

export const useTablesForeignKeys = <T = TableFkRelationships[][]>(
  { tables, source }: UseTablesForeignKeysProps,
  options?: UseQueryOptions<TableFkRelationships[][], unknown, T>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getTablesForeignKeysQueryKey(source.name, tables),
    queryFn: async () => {
      if (!tables.length) {
        return [];
      }

      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getFKRelationships) {
        return [];
      }

      return Promise.all(
        tables.map((table) =>
          dataSource.introspection.getFKRelationships!({
            endpoints,
            fetchJson,
            table,
            dataSourceName: source.name,
          }),
        ),
      );
    },
    ...defaultQueryOptions,
    ...options,
  });
};
