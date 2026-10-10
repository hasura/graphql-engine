import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { Capabilities } from '@hasura/dc-api-types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { getDatabaseMethods } from '../../driver';

type UseDatabaseCapabilitiesArgs = {
  source: QualifiedDataSource | undefined;
};

export const useDriverCapabilities = <FinalResult = Capabilities>(
  { source }: UseDatabaseCapabilitiesArgs,
  options?: Omit<
    UseQueryOptions<Capabilities, unknown, FinalResult>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<Capabilities, unknown, FinalResult>({
    ...options,
    queryKey: [source?.name, 'capabilities'],
    queryFn: async () => {
      if (!source) {
        return {};
      }

      const databaseMethods = getDatabaseMethods(source.kind);
      return databaseMethods.introspection.getDriverCapabilities({
        endpoints,
        fetchJson,
        driver: source.kind,
      });
    },
    staleTime: Infinity,
    refetchOnWindowFocus: false,
    enabled: Boolean(source) && options?.enabled !== false,
  });
};
