import { WebSocketLink } from 'apollo-link-ws';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from "apollo-cache-inmemory";

const makeApolloClient = (token) => {

  const link = new WebSocketLink({
    uri: `wss://learn.hasura.io/graphql`,
    options: {
      reconnect: true,
      connectionParams: {
        headers: {
          Authorization: `Bearer ${token}`
        }
      }
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
