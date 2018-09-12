import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import ws from 'ws';
import { makeRemoteExecutableSchema, introspectSchema } from 'graphql-tools';
import fetch from 'node-fetch';
import { split } from 'apollo-link';
import { getMainDefinition } from 'apollo-utilities';

const { HASURA_GRAPHQL_ENGINE_AUTH_HOOK } = process.env;

// util function to fetch and create remote schema
export const getRemoteSchema = async (uri, headers) => {
  const link = makeHttpAndWsLink(uri, headers);
  const schema = await introspectSchema(link);
  return makeRemoteExecutableSchema({
    schema,
    link
  });
};

/* create an apollo-link instance that makes
 WS connection for subscriptions and
 HTTP connection for queries andm utations
*/
const makeHttpAndWsLink = (uri, headers) => {

  // Create an http link:
  const httpLink = new HttpLink({
    uri,
    fetch,
    headers
  });


  // Create a WebSocket link:
  const wsLink = new WebSocketLink(new SubscriptionClient(
    uri,
    {
      reconnect: true,
      connectionParams: {
        headers
      }
    },
    ws
  ));

  // chose the link to use based on operation
  const link = split(
    // split based on operation type
    ({ query }) => {
      const { kind, operation } = getMainDefinition(query);
      return kind === 'OperationDefinition' && operation === 'subscription';
    },
    wsLink,
    httpLink,
  );


  return link;
};

