import { ApolloClient, InMemoryCache, split, HttpLink } from "@apollo/client";
import { getMainDefinition  } from "@apollo/client/utilities";
import { WebSocketLink } from "@apollo/client/link/ws";
import { wsurl, httpurl } from './constants';

const wsLink = new WebSocketLink({
  uri: wsurl,
  options: {
    reconnect: true
  }
});

const httpLink = new HttpLink({
  uri: httpurl,
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
  cache: new InMemoryCache()
});

export default client;
