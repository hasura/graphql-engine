import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { getDatabaseMethods, TriggerFunction } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

/** Functions in the source that return `trigger` (usable in CREATE TRIGGER). */
export const useTriggerFunctions = <T = TriggerFunction[]>(
  { source }: { source: QualifiedDataSource },
  options?: Omit<
    UseQueryOptions<TriggerFunction[], unknown, T>,
    'queryKey' | 'queryFn'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source.name, 'GET_TRIGGER_FUNCTIONS'],
    queryFn: async () => {
      const dataSource = getDatabaseMethods(source.kind);
      if (!dataSource.introspection.getTriggerFunctions) return [];
      return dataSource.introspection.getTriggerFunctions({
        endpoints,
        fetchJson,
        dataSourceName: source.name,
      });
    },
    ...defaultQueryOptions,
    ...options,
  });
};
