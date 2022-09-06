import React from "react";
import { createRoot } from "react-dom/client";
import App from "./App";
import {
  ApolloClient,
  InMemoryCache,
  ApolloProvider,
  split,
  HttpLink,
} from "@apollo/client";
import { getMainDefinition } from "@apollo/client/utilities";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import reportWebVitals from "./reportWebVitals";

const scheme = (proto) => {
  return window.location.protocol === "https:" ? `${proto}s` : proto;
};

const HASURA_GRAPHQL_ENGINE_HOSTNAME =
  process.env.REACT_APP_HASURA_GRAPHQL_ENGINE_HOSTNAME || "localhost:8080";
export const GRAPHQL_ENDPOINT = `${scheme(
  "http"
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;
export const WEBSOCKET_ENDPOINT = `${scheme(
  "ws"
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;

const httpLink = new HttpLink({ uri: GRAPHQL_ENDPOINT });

const wsLink = new GraphQLWsLink(
  createClient({
    url: WEBSOCKET_ENDPOINT,
  })
);

const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === "OperationDefinition" && operation === "subscription";
  },
  wsLink,
  httpLink
);

// Instantiate client
const client = new ApolloClient({
  link,
  cache: new InMemoryCache({
    addTypename: false,
  }),
});

const container = document.getElementById("root");
const root = createRoot(container);

root.render(
  <React.StrictMode>
    <ApolloProvider client={client}>
      <App />
    </ApolloProvider>
  </React.StrictMode>
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
