import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from "apollo-cache-inmemory";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";

const GRAPHQL_ENGINE_ENDPOINT = 'learn.hasura.io/graphql'

const makeApolloClient = (token) => {
  const link = new HttpLink({
    uri: `https://learn.hasura.io/graphql`,
    headers: {
      Authorization: `Bearer ${token}`
    }
  });

  const cache = new InMemoryCache()

  const client = new ApolloClient({
    link,
    cache
  });

  return client;
}

export default makeApolloClient;
