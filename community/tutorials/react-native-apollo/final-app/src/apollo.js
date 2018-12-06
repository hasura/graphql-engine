import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from "apollo-cache-inmemory";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";

const GRAPHQL_ENGINE_ENDPOINT = 'hasura-todo-test.herokuapp.com/v1alpha1/graphql'

const makeApolloClient = (token) => {
  const httpLink = new HttpLink({
    uri: `https://${GRAPHQL_ENGINE_ENDPOINT}`,
    headers: {
      Authorization: `Bearer ${token}`
    }
  });
  const wsLink = new WebSocketLink({
    uri: `wss://${GRAPHQL_ENGINE_ENDPOINT}`,
    options: {
      reconnect: true,
      connectionParams: {
        headers: {
          Authorization: `Bearer ${token}`
        }
      }
    }
  });

  const link = split(
    // split based on operation type
    ({ query }) => {
      const { kind, operation } = getMainDefinition(query);
      return kind === 'OperationDefinition' && operation === 'subscription';
    },
    wsLink,
    httpLink,
  );
  const client = new ApolloClient({
    link,
    cache: new InMemoryCache(),
  });

  return client;
}

export default makeApolloClient;
