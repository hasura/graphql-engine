import React, { Component } from 'react';
import { ApolloClient } from "apollo-boost";
import { ApolloProvider } from "react-apollo";
import { BrowserRouter as Router, Switch, Link, Route, Redirect } from 'react-router-dom';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { split } from 'apollo-link';
import { getMainDefinition } from 'apollo-utilities';
import Login from './components/Login';
import Dashboard from './components/Dashboard';
import { vars } from './env';
import './App.css';

// Create an http link:
const httpLink = new HttpLink({
  uri: vars.GRAPHQL_ENDPOINT,
});

// Create a WebSocket link:
const wsLink = new WebSocketLink({
  uri: vars.GRAPHQL_SUBS_ENDPOINT,
  options: {
    reconnect: true
  }
});

// using the ability to split links, you can send data to each link
// depending on what kind of operation is being sent
const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === 'OperationDefinition' && operation === 'subscription';
  },
  wsLink,
  httpLink,
);

// Instantiate client
const client = new ApolloClient({
  link,
  cache: new InMemoryCache()
})

class App extends Component {
  render() {
    return (
      <ApolloProvider client={client}>
        <div className="App container-fluid">
          <Switch>
            <Route path="/" exact={true} component={Login} />
            <Route path="/dashboard/:name/:userID" exact={true} component={Dashboard} />
          </Switch>
        </div>
      </ApolloProvider>
    );
  }
}

export default App;
