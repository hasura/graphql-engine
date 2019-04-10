import ApolloClient from "apollo-client";
import { HttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";
import { WebSocketLink } from "apollo-link-ws";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";
import { SubscriptionClient } from "subscriptions-transport-ws";

/*
const GRAPHQL_URL = 'https://react-apollo-todo.demo.hasura.app/v1alpha1/graphql';
const REALTIME_GRAPHQL_URL = 'wss://react-apollo-todo.demo.hasura.app/v1alpha1/graphql'
*/

const GRAPHQL_URL = 'https://backend.graphql-tutorials.org/v1alpha1/graphql';
const REALTIME_GRAPHQL_URL = 'wss://backend.graphql-tutorials.org/v1alpha1/graphql'

const getHeaders = () => {
  const headers = {};
  const token = window.localStorage.getItem('apollo-token');
  if (token) {
    headers.authorization = `Bearer ${token}`;
  }
  return headers;
};

// Create an http link:
const httpLink = new HttpLink({
  uri: GRAPHQL_URL,
  fetch,
  headers: getHeaders()
});

// Create a WebSocket link:
const wsLink = new WebSocketLink(
  new SubscriptionClient(REALTIME_GRAPHQL_URL, {
    reconnect: true,
    timeout: 30000,
    connectionParams: () => {
      return { headers: getHeaders() };
    },
    connectionCallback: err => {
      if (err) {
        wsLink.subscriptionClient.close(false, false);
      }
    }
  })
);

// chose the link to use based on operation
const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === "OperationDefinition" && operation === "subscription";
  },
  wsLink,
  httpLink
);

const createProvider = () => {
  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache({
      addTypename: true
    })
  });
  return client;
}

export default createProvider;


