import endpoints from '../../Endpoints';
import { isCloudConsole, requestJson } from '@hasura/shared/utils';
import { print, DocumentNode } from 'graphql/language';
import { GraphQLError } from 'graphql/error';
import { createClient } from 'graphql-ws';

const getGraphqlSubscriptionsClient = (
  url: string,
  headers: Record<string, string>,
) => {
  return createClient({
    url,
    connectionParams: {
      headers: {
        ...headers,
      },
      lazy: true,
      shouldRetry: () => true,
    },
  });
};

export const createControlPlaneClient = (
  endpoint: string = endpoints.luxDataGraphql,
  headers: Record<string, string> = {
    'content-type': 'application/json',
    'hasura-client-name': 'hasura-console',
  },
) => {
  const subscriptionsClient = isCloudConsole(window.__env)
    ? getGraphqlSubscriptionsClient(endpoints.luxDataGraphqlWs, headers)
    : null;

  const query = <
    ResponseType = Record<string, any>,
    VariablesType = Record<string, any>,
  >(
    queryDoc: DocumentNode,
    variables: VariablesType,
  ): Promise<ResponseType> => {
    return requestJson<ResponseType>(endpoint, {
      method: 'POST',
      headers,
      body: JSON.stringify({
        query: print(queryDoc),
        variables: variables || {},
      }),
      credentials: 'include',
    });
  };

  const subscribe = <
    ResponseType = Record<string, any>,
    VariablesType extends Record<string, any> = Record<string, any>,
  >(
    queryDoc: DocumentNode,
    variables: VariablesType,
    dataCallback: (data: ResponseType) => void,
    errorCallback: (error: GraphQLError) => void,
  ) => {
    if (!subscriptionsClient) {
      return { unsubscribe: () => null };
    }

    const unsubscribe = subscriptionsClient.subscribe(
      {
        query: print(queryDoc),
        variables,
      },
      {
        next: (data: any) => {
          dataCallback(data.data as ResponseType);
        },
        error: (error: Error) => {
          errorCallback(new GraphQLError(error.message));
        },
        complete: () => {},
      },
    );

    return { unsubscribe };
  };

  return {
    query,
    subscribe,
  };
};

export const controlPlaneClient = createControlPlaneClient();
export type ControlPlaneClient = typeof controlPlaneClient;
