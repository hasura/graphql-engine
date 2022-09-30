import ApolloClient from 'apollo-client';
import { HttpLink } from 'apollo-link-http';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { split } from 'apollo-link';
import { WebSocketLink } from 'apollo-link-ws';
import { getMainDefinition } from 'apollo-utilities';

const getWsFromHttp = url => {
  return url.replace(/(http)(s)?:\/\//, 'ws$2://');
};

const makeApolloClient = (url, headers, linkWS = true) => {
  const httpLink = new HttpLink({
    uri: url,
    credentials: 'include',
    fetch,
    headers,
  });

  let link = null;
  let wsLink = null;
  if (linkWS) {
    wsLink = new WebSocketLink({
      uri: getWsFromHttp(url),
      options: {
        reconnect: true,
        lazy: true,
        connectionParams: () => {
          return {
            headers,
          };
        },
        timeout: 60000,
      },
    });

    // to ensure the duration to attempt to reconnect is set properly https://github.com/apollographql/subscriptions-transport-ws/issues/377
    // wsLink.subscriptionClient.maxConnectTimeGenerator.duration = () => wsLink.subscriptionClient.maxConnectTimeGenerator.max;
    link = split(
      // split based on operation type
      ({ query }) => {
        const { kind, operation } = getMainDefinition(query);
        return kind === 'OperationDefinition' && operation === 'subscription';
      },
      wsLink,
      httpLink
    );
  } else {
    link = httpLink;
  }

  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache({
      addTypename: false,
    }),
  });
  return { client, wsLink };
};
export { makeApolloClient };
