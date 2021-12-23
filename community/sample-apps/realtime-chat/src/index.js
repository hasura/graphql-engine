import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import {
  ApolloClient,
  InMemoryCache,
  ApolloProvider,
  split,
  HttpLink,
} from '@apollo/client';
import { getMainDefinition } from '@apollo/client/utilities';
import { ApolloLink, Observable } from '@apollo/client/core';

import { print } from 'graphql';
import { createClient } from 'graphql-ws';
import reportWebVitals from './reportWebVitals';

// Create graphql-ws Apollo adapter
class WebSocketLink extends ApolloLink {
  constructor(options) {
    super();
    this.client = createClient(options);
  }

  request(operation) {
    return new Observable((sink) => {
      return this.client.subscribe(
        { ...operation, query: print(operation.query) },
        {
          next: sink.next.bind(sink),
          complete: sink.complete.bind(sink),
          error: (err) => {
            if (Array.isArray(err))
              // GraphQLError[]
              return sink.error(
                new Error(err.map(({ message }) => message).join(', '))
              );

            if (err instanceof CloseEvent)
              return sink.error(
                new Error(
                  `Socket closed with event ${err.code} ${err.reason || ''}` // reason will be available on clean closes only
                )
              );

            return sink.error(err);
          },
        }
      );
    });
  }
}

const scheme = (proto) => {
  return window.location.protocol === 'https:' ? `${proto}s` : proto;
};
const HASURA_GRAPHQL_ENGINE_HOSTNAME = 'localhost:8080';
export const GRAPHQL_ENDPOINT = `${scheme(
  'http'
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;
export const WEBSOCKET_ENDPOINT = `${scheme(
  'ws'
)}://${HASURA_GRAPHQL_ENGINE_HOSTNAME}/v1/graphql`;

// Make WebSocketLink with appropriate url
const mkWsLink = (uri) => {
  const subClient = {
    url: WEBSOCKET_ENDPOINT,
    options: {
      reconnect: true,
    },
  };
  return new WebSocketLink(subClient);
};

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
    addTypename: false,
  }),
});

ReactDOM.render(
  <React.StrictMode>
    <ApolloProvider client={client}>
      <App />
    </ApolloProvider>
  </React.StrictMode>,
  document.getElementById('root')
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
