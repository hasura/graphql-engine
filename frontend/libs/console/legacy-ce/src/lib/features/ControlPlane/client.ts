import endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { getGraphqlSubscriptionsClient } from '@/utils/graphqlSubscriptions';
import { print, DocumentNode } from 'graphql/language';
import { GraphQLError } from 'graphql/error';

export const createControlPlaneClient = (
  endpoint: string = endpoints.luxDataGraphql,
  headers = {
    'content-type': 'application/json',
    'hasura-client-name': 'hasura-console',
  }
) => {
  const subscriptionsClient = getGraphqlSubscriptionsClient(
    endpoints.luxDataGraphqlWs,
    headers
  );

  const query = <
    ResponseType = Record<string, any>,
    VariablesType = Record<string, any>
  >(
    queryDoc: DocumentNode,
    variables: VariablesType
  ): Promise<ResponseType> => {
    return Api.post<ResponseType>({
      url: endpoint,
      headers,
      body: {
        query: print(queryDoc),
        variables: variables || {},
      },
      credentials: 'include',
    });
  };

  const subscribe = <
    ResponseType = Record<string, any>,
    VariablesType = Record<string, any>
  >(
    queryDoc: DocumentNode,
    variables: VariablesType,
    dataCallback: (data: ResponseType) => void,
    errorCallback: (error: GraphQLError) => void
  ) => {
    const request = subscriptionsClient.request({
      query: queryDoc,
      variables,
    });
    const { unsubscribe } = request.subscribe({
      next: (data: any) => {
        dataCallback(data.data as ResponseType);
      },
      error: (error: Error) => {
        errorCallback(new GraphQLError(error.message));
      },
    });
    return { unsubscribe };
  };

  return {
    query,
    subscribe,
  };
};

export const controlPlaneClient = createControlPlaneClient();
export type ControlPlaneClient = typeof controlPlaneClient;
