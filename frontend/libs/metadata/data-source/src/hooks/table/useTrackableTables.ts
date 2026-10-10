import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getDatabaseMethods, IntrospectedTable } from '../../driver';
import { getTrackableTablesQueryKey } from '../../types/queryKey';
import { Source } from '@hasura/shared/types';

export const useTrackableTables = <FinalResult = IntrospectedTable[]>(
  {
    source,
  }: {
    source: Source | undefined;
  },
  options?: Omit<
    UseQueryOptions<IntrospectedTable[], unknown, FinalResult>,
    'queryKey'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<IntrospectedTable[], unknown, FinalResult>({
    queryKey: getTrackableTablesQueryKey(source?.name ?? ''),
    queryFn: async () => {
      if (!source?.kind) {
        return [];
      }

      const databaseMethods = getDatabaseMethods(source?.kind);
      const getTrackableTables =
        databaseMethods.introspection?.getTrackableTables;

      if (getTrackableTables) {
        return getTrackableTables({
          dataSourceName: source.name,
          configuration: source.configuration,
          endpoints,
          fetchJson,
        });
      }

      return [];
    },
    ...defaultQueryOptions,
    ...options,
    enabled: Boolean(source) || options?.enabled !== false,
  });
};
