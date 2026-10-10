import { useMutation } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { runGraphQL } from '../../api';
import { useAppContext } from '@hasura/shared/context';

export function useGraphQLMutation({
  operationName,
  headers,
  onError,
  onSuccess,
}: {
  operationName: string;
  headers?: Record<string, string>;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useMutation<
    any, // runGraphQL returns any
    Error,
    { query: string; resultPath: string }
  >({
    mutationFn: async ({ query }) => {
      const res = await runGraphQL({
        url: endpoints.graphQLUrl,
        headers,
        fetchJson,
        body: {
          operationName,
          query,
        },
      });
      return res.data;
    },
    onSuccess,
    onError,
  });
}
