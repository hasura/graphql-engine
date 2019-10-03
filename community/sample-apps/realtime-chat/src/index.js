import React from 'react';
import ReactDOM from 'react-dom';
import { ApolloProvider } from 'react-apollo';
import App from './App';
import ApolloClient from 'apollo-client';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { split } from 'apollo-link';
import { getMainDefinition } from 'apollo-utilities';

const scheme = (proto) => {
  return window.location.protocol === 'https:' ? `${proto}s` : proto;
}
const HASURA_GRAPHQL_ENGINE_HOSTNAME = 'realtime-chat.demo.hasura.app';
export const GRAPHQL_ENDPOINT = `${scheme('http')}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;
export const WEBSOCKET_ENDPOINT = `${scheme('ws')}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;

// Make WebSocketLink with appropriate url
const mkWsLink = (uri) => {
  const splitUri = uri.split('//');
  const subClient = new SubscriptionClient(
    WEBSOCKET_ENDPOINT,
    { reconnect: true }
  );
  return new WebSocketLink(subClient);
}

// Make HttpLink
const httpLink = new HttpLink({ uri: GRAPHQL_ENDPOINT });
const wsLink = mkWsLink(GRAPHQL_ENDPOINT);
const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === 'OperationDefinition' && operation === 'subscription';
  },
  wsLink,
  httpLink
);

// Instantiate client
const client = new ApolloClient({
  link,
  cache: new InMemoryCache({
    addTypename: false
  })
})

ReactDOM.render(
  (<ApolloProvider client={client}>
    <App />
  </ApolloProvider>),
  document.getElementById('root')
);
