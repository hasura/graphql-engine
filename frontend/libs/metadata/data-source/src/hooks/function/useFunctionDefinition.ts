import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, TableFunction } from '@hasura/shared/types';
import { getDatabaseMethods, GetFunctionDefinitionResult } from '../../driver';

const GET_FUNCTION_DEFINITION_QUERY_KEY = 'GET_FUNCTION_DEFINITION';

const getFunctionDefinitionQueryKey = (
  dataSourceName: string,
  func: TableFunction | undefined,
) => {
  return [dataSourceName, GET_FUNCTION_DEFINITION_QUERY_KEY, func];
};

export const useFunctionDefinition = (
  {
    source,
    func,
  }: {
    source: Partial<QualifiedDataSource>;
    func: TableFunction | undefined;
  },
  options?: UseQueryOptions<
    GetFunctionDefinitionResult | null,
    unknown,
    GetFunctionDefinitionResult | null
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getFunctionDefinitionQueryKey(source.name ?? '', func),
    queryFn: async () => {
      if (!source.kind || !source.name || !func) {
        return null;
      }

      const database = getDatabaseMethods(source.kind);
      if (!database.introspection.getFunctionDefinition) {
        return null;
      }

      return database.introspection.getFunctionDefinition({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
        func,
      });
    },
    ...options,
    enabled:
      Boolean(source?.name && source?.kind) &&
      Boolean(func) &&
      options?.enabled !== false,
  });
};
