import { AxiosInstance } from 'axios';
import { introspectionQuery } from './introspectionQuery';

export interface NetworkArgs {
  httpClient: AxiosInstance;
}

type Args = { operationName: string; query: string } & NetworkArgs;

export const runGraphQL = async ({
  operationName,
  query,
  httpClient,
}: Args) => {
  try {
    const result = await httpClient.post('v1/graphql', {
      query,
      operationName,
    });
    return result.data;
  } catch (err) {
    throw err;
  }
};

export const runIntrospectionQuery = async ({ httpClient }: NetworkArgs) => {
  return runGraphQL({
    operationName: 'IntrospectionQuery',
    query: introspectionQuery,
    httpClient,
  });
};
