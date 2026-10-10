import { GraphQLWsLink } from '@apollo/client/link/subscriptions';
import {
  ApolloClient,
  ApolloLink,
  HttpLink,
  InMemoryCache,
} from '@apollo/client';
import { createClient } from 'graphql-ws';
import { OperationTypeNode } from 'graphql';
import { replaceURLHttpToWs } from '@hasura/shared/utils';

export const makeApolloClient = (
  url: string,
  headers: Record<string, string> | undefined,
) => {
  const httpLink = new HttpLink({
    uri: url,
    credentials: 'include',
    fetch,
    headers,
  });

  let link = httpLink;
  const wsClient = createClient({
    url: replaceURLHttpToWs(url),
    connectionParams: () => {
      return {
        headers,
      };
    },
    lazy: true,
    retryAttempts: 10,
    shouldRetry: () => true,
  });

  // to ensure the duration to attempt to reconnect is set properly https://github.com/apollographql/subscriptions-transport-ws/issues/377
  // wsLink.subscriptionClient.maxConnectTimeGenerator.duration = () => wsLink.subscriptionClient.maxConnectTimeGenerator.max;
  link = ApolloLink.split(
    ({ operationType }) => {
      return operationType === OperationTypeNode.SUBSCRIPTION;
    },
    new GraphQLWsLink(wsClient),
    httpLink,
  );

  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache(),
  });

  return { client, wsClient };
};

export type ApolloClientOutput = ReturnType<typeof makeApolloClient>;

export const disposeApolloClient = async (
  persistedClient: ApolloClientOutput,
) => {
  // Close socket connection which will also unregister subscriptions on the server-side.
  persistedClient.wsClient.dispose();
  persistedClient.client.stop();
};
