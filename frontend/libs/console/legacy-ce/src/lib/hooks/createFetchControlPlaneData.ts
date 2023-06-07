import Endpoints from '../Endpoints';
import { Api } from './apiUtils';
import { GraphQLError } from 'graphql';

/**
 * Calls hasura cloud data service with provided query and variables. Uses the common `fetch` api client.
 * Returns a promise which either resolves the data or throws an error. Optionally pass a transform function
 * to transform the response data. This can be transformed into a hook, and directly use headers from
 * `useAppSelector` hook if required. This can also be passed to react query as the `queryFn` if required.
 */
export function controlPlaneDataApiClient<
  ResponseData,
  TransformedData = ResponseData
>(
  query: string,
  variables: Record<string, string>,
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
type ErrorMessage = string;

type FetchControlPlaneData<RESPONSE_DATA> = (overrides?: {
  variables: Record<string, string>;
}) => Promise<RESPONSE_DATA | ErrorMessage>;

export function createFetchControlPlaneData<RESPONSE_DATA>(opts: {
  query: string;
  variables?: Record<string, string>;
}): FetchControlPlaneData<RESPONSE_DATA> {
  const { query, variables } = opts;

  return async (overrides = { variables: {} }) => {
    try {
      const response = await controlPlaneDataApiClient<
        RESPONSE_DATA | { errors: GraphQLError[] }
      >(
        query,
        { ...variables, ...overrides?.variables },
        { 'content-type': 'application/json' }
      );

      if (response && typeof response === 'object' && 'errors' in response) {
        const errorMessage = response.errors?.[0]?.message ?? '';

        return errorMessage;
      }

      return response;
    } catch (error) {
      console.error(error);
      return 'unexpected network error';
    }
  };
}
