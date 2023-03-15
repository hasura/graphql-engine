import { useMutation } from 'react-query';
import { runGraphQL } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { AxiosResponseHeaders } from 'axios';

export function useGraphQLMutation({
  operationName,
  headers,
  onError,
  onSuccess,
}: {
  operationName: string;
  headers?: AxiosResponseHeaders;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) {
  const httpClient = useHttpClient();
  return useMutation<
    any, // runGraphQL returns any
    Error,
    { query: string; resultPath: string }
  >(
    async ({ query }) => {
      const res = await runGraphQL({
        httpClient,
        operationName,
        query,
        headers,
      });
      return res.data;
    },
    {
      onSuccess,
      onError,
    }
  );
}
