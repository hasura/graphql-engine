import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getDatabaseMethods } from '../../driver';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getTableCommentQueryKey } from '../../types/queryKey';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useTableComment = (
  {
    source,
    table,
  }: {
    source: QualifiedDataSource;
    table: Table;
  },
  options?: UseQueryOptions<string | undefined, unknown>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<string | undefined, unknown>({
    queryKey: getTableCommentQueryKey(source.name, table),
    queryFn: async () => {
      const databaseMethods = getDatabaseMethods(source.kind);
      if (!databaseMethods.introspection.getTableComment) {
        return undefined;
      }

      return databaseMethods.introspection.getTableComment({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
        table,
      });
    },
    ...defaultQueryOptions,
    ...options,
  });
};
