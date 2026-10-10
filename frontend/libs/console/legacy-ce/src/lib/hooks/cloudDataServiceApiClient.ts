import { requestJson } from '@hasura/shared/utils';
import Endpoints from '../Endpoints';

/**
 * Calls hasura cloud data service with provided query and variables. Uses the common `fetch` api client.
 * Returns a promise which either resolves the data or throws an error. Optionally pass a transform function
 * to transform the response data. This can be transformed into a hook, and directly use headers from
 * `useAppSelector` hook if required. This can also be passed to react query as the `queryFn` if required.
 */
export function cloudDataServiceApiClient<
  ResponseData,
  TransformedData = ResponseData,
>(
  query: string,
  variables: Record<string, unknown>,
  headers: Record<string, string>,
  transformFn?: (data: ResponseData) => TransformedData,
): Promise<TransformedData> {
  return requestJson<ResponseData>(Endpoints.luxDataGraphql, {
    method: 'POST',
    headers,
    body: JSON.stringify({
      query,
      variables: variables || {},
    }),
    credentials: 'include',
  }).then((data) =>
    transformFn ? transformFn(data) : (data as unknown as TransformedData),
  );
}
