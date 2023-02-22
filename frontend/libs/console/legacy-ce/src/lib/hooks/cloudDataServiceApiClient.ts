import Endpoints from '../Endpoints';
import { Api } from './apiUtils';

/**
 * Calls hasura cloud data service with provided query and variables. Uses the common `fetch` api client.
 * Returns a promise which either resolves the data or throws an error. Optionally pass a transform function
 * to transform the response data. This can be transformed into a hook, and directly use headers from
 * `useAppSelector` hook if required. This can also be passed to react query as the `queryFn` if required.
 */
export function cloudDataServiceApiClient<
  ResponseData,
  TransformedData = ResponseData
>(
  query: string,
  variables: Record<string, unknown>,
  headers: Record<string, string>,
  transformFn?: (data: ResponseData) => TransformedData
): Promise<TransformedData> {
  return Api.post<ResponseData, TransformedData>(
    {
      url: Endpoints.luxDataGraphql,
      headers,
      body: {
        query,
        variables: variables || {},
      },
      credentials: 'include',
    },
    transformFn
  );
}
