import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, StoredProcedure } from '@hasura/shared/types';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getDatabaseMethods } from '../../driver';

export const useStoredProcedures = <FinalResult = StoredProcedure[]>(
  {
    source,
  }: {
    // `source` is optional so the query can be mounted before a data source has
    // been selected (callers gate the fetch with `enabled: !!source`); the
    // query key and function both handle the absent case.
    source: QualifiedDataSource | undefined;
  },
  options?: Omit<
    UseQueryOptions<StoredProcedure[], unknown, FinalResult>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<StoredProcedure[], unknown, FinalResult>({
    ...defaultQueryOptions,
    ...options,
    queryKey: [source?.name, 'stored_procedures'],
    queryFn: async () => {
      if (!source) {
        return [];
      }
      const databaseMethods = getDatabaseMethods(source.kind);
      if (!databaseMethods.introspection.getStoredProcedures) {
        return [];
      }

      return databaseMethods.introspection.getStoredProcedures({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
      });
    },
  });
};
