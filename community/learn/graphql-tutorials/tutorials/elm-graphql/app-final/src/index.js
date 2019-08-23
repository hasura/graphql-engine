import './main.css';
import { Elm } from './Main.elm';

/* */
import ApolloClient from "apollo-client";
import { split } from 'apollo-link';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { getMainDefinition } from 'apollo-utilities';
import { InMemoryCache } from "apollo-cache-inmemory";

import gql from 'graphql-tag'

// Replace it with your graphql url
const GRAPHQL_URI = 'learn.hasura.io/graphql';

const getClient = (token) => {
  // Create an http link:
  const httpLink = new HttpLink({
    uri: `https://${GRAPHQL_URI}`
  });

  // Create a WebSocket link:
  const wsLink = new WebSocketLink({
    uri: `wss://${GRAPHQL_URI}`,
    options: {
      reconnect: true
      , connectionParams: {
        headers: {
          Authorization: `Bearer ${ token }`
        }
      }
    }
  });

  // using the ability to split links, you can send data to each link
  // depending on what kind of operation is being sent
  const link = split(
    // split based on operation type
    ({ query }) => {
      const definition = getMainDefinition(query);
      return (
        definition.kind === 'OperationDefinition' &&
        definition.operation === 'subscription'
      );
    },
    wsLink,
    httpLink,
  );
  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache({
      addTypename: true
    })
  });
  return client;
};

document.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    node: document.getElementById("root")
  });
  app.ports.createSubscriptionToPublicTodos.subscribe(function(data) {
    /* Initiate subscription request */
    var [ data, authToken ] = data;
    if (authToken.length > 0) {
      // app.ports.creatingSubscriptionToTasks.send(1);
      getClient(authToken).subscribe({
        query: gql`${data}`,
        variables: {}
      }).subscribe({
        next(resp) {
          app.ports.gotRecentPublicTodoItem.send(resp);
        },
        error(err) {
          console.log('error is');
          console.log(err);
        }
      });
    }
  });
  app.ports.createSubscriptionToOnlineUsers.subscribe(function(data) {
    /* Initiate subscription request */
    var [ data, authToken ] = data;
    if (authToken.length > 0) {
      getClient(authToken).subscribe({
        query: gql`${data}`,
        variables: {}
      }).subscribe({
        next(resp) {
          app.ports.gotOnlineUsers.send(resp);
        },
        error(err) {
          console.log('error is');
          console.log(err);
        }
      });
    }
  });
})

