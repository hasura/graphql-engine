import ApolloClient from "apollo-client";
import { HttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";
import { WebSocketLink } from "apollo-link-ws";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";
import { SubscriptionClient } from "subscriptions-transport-ws";
import { setContext } from "apollo-link-context";

import { GRAPHQL_URL, REALTIME_GRAPHQL_URL } from "./utils/constants";

const getHeaders = () => {
  const token = localStorage.getItem("auth0:id_token");
  const headers = {
    authorization: token ? `Bearer ${token}` : ""
  };
  return headers;
};

const makeApolloClient = () => {
  const authLink = setContext((_, { headers }) => {
    const token = localStorage.getItem("auth0:id_token");
    return {
      headers: {
        ...headers,
        authorization: token ? `Bearer ${token}` : ""
      }
    };
  });

  const token = localStorage.getItem("auth0:id_token");
  // Create an http link:
  const httpLink = new HttpLink({
    uri: GRAPHQL_URL,
    fetch,
    headers: getHeaders(token)
  });

  // Create a WebSocket link:
  const wsLink = new WebSocketLink(
    new SubscriptionClient(REALTIME_GRAPHQL_URL, {
      reconnect: true,
      timeout: 30000,
      connectionParams: {
        headers: getHeaders(token)
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

  const client = new ApolloClient({
    link: authLink.concat(link),
    cache: new InMemoryCache({
      addTypename: true
    })
  });

  return client;
};

export default makeApolloClient;
