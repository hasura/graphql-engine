import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, GetViewDefinitionResult } from '../../driver';
import { defaultQueryOptions } from '@hasura/metadata/api';

const GET_VIEW_DEFINITION_QUERY_KEY = 'GET_VIEW_DEFINITION';

const getViewDefinitionQueryKey = (
  dataSourceName: string,
  table: Table | undefined,
) => {
  return [dataSourceName, GET_VIEW_DEFINITION_QUERY_KEY, table];
};

export const useViewDefinition = (
  {
    source,
    table,
  }: {
    source: Partial<QualifiedDataSource>;
    table: Table | undefined;
  },
  options?: UseQueryOptions<
    GetViewDefinitionResult | null,
    unknown,
    GetViewDefinitionResult | null
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getViewDefinitionQueryKey(source.name ?? '', table),
    queryFn: async () => {
      if (!source.kind || !source.name || !table) {
        return null;
      }

      const database = getDatabaseMethods(source.kind);
      if (!database.introspection.getViewDefinition) {
        return null;
      }

      return database.introspection.getViewDefinition({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
        table,
      });
    },
    ...defaultQueryOptions,
    ...options,
    enabled:
      Boolean(source?.name && source?.kind) &&
      Boolean(table) &&
      options?.enabled !== false,
  });
};
