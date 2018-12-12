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
import Home from './components/Home';
import './App.css';

const httpLink = new HttpLink({
  uri: "",
});

// Create a WebSocket link:
const wsLink = new WebSocketLink({
  uri: ``,
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
            <Route path="/" exact={true} component={Home} />
          </Switch>
        </div>
      </ApolloProvider>
    );
  }
}

export default App;
