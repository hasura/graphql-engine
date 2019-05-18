import Vue from "vue";

import { ApolloClient } from "apollo-client";
import { HttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";

import { WebSocketLink } from "apollo-link-ws";
import { getMainDefinition } from "apollo-utilities";
import { split } from "apollo-link";


import VueApollo from "vue-apollo";
// Http endpoint
const httpLink = new HttpLink({
  uri: "https://realtime-chat.demo.hasura.app/v1/graphql"
})

const wsLink = new WebSocketLink({
  uri: "wss://realtime-chat.demo.hasura.app/v1/graphql",
  options: {
    reconnect: true
  }
});

const link = split(
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === "OperationDefinition" && operation === "subscription";
  },
  wsLink,
  httpLink
);


const apolloClient = new ApolloClient({
  link,
  cache: new InMemoryCache(),
  connectToDevTools: true
});

Vue.use(VueApollo);

// Call this in the Vue app file
export function createProvider() {
  return new VueApollo({
    defaultClient: apolloClient,
    defaultOptions: {
      $loadingKey: "loading"
    }
  });
}
