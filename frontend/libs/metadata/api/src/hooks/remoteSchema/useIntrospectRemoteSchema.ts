import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { buildClientSchema, GraphQLSchema, IntrospectionQuery } from 'graphql';
import { useAppContext } from '@hasura/shared/context';

export const INTROSPECT_REMOTE_SCHEMA_QUERY_KEY = 'INTROSPECT_REMOTE_SCHEMA';

type Options<FinalResult = any> = {
  staleTime?: number;
  enabled?: boolean;
  select?: (m: unknown) => FinalResult;
};

// custom hook for introspecting remote schema
export const useIntrospectRemoteSchema = (
  remoteSchemaName: string,
  options?: Options,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<GraphQLSchema>({
    queryKey: [INTROSPECT_REMOTE_SCHEMA_QUERY_KEY, remoteSchemaName],
    queryFn: () => {
      return fetchJson<{ data: IntrospectionQuery }>(endpoints.metadata, {
        method: 'POST',
        body: JSON.stringify({
          type: 'introspect_remote_schema',
          args: {
            name: remoteSchemaName,
          },
        }),
      }).then((response) => buildClientSchema(response.data));
    },
    ...options,
    enabled: options?.enabled !== false && Boolean(remoteSchemaName),
  });
};
