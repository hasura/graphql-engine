import { buildClientSchema, GraphQLSchema } from 'graphql';
import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { runIntrospectionQuery } from '../../api';

export const getGraphQLQueryPayload = (
  query: string,
  variables: Record<string, any> | null | undefined,
  operationName?: string,
) => ({
  query,
  variables,
  operationName,
});

export const INTROSPECT_SCHEMA_QUERY_KEY = 'INTROSPECT_SCHEMA';

type Options<FinalResult = any> = {
  staleTime?: number;
  enabled?: boolean;
  select?: (m: unknown) => FinalResult;
};

export const useIntrospectSchema = (options?: Options) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<GraphQLSchema>({
    queryKey: [INTROSPECT_SCHEMA_QUERY_KEY],
    queryFn: () => {
      return runIntrospectionQuery({
        url: endpoints.graphQLUrl,
        fetchJson,
      }).then((response) => buildClientSchema(response));
    },
    ...options,
  });
};
