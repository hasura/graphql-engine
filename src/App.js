import React from 'react';
import { ApolloProvider } from 'react-apollo';
import AppRouter from './AppRouter';
import ApolloClient from 'apollo-client';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { split } from 'apollo-link';
import { getMainDefinition } from 'apollo-utilities';

const GRAPHQL_ENDPOINT = "https://sureeee.herokuapp.com/v1alpha1/graphql";

const mkWsLink = (uri) => {
  const splitUri = uri.split('//');
  const subClient = new SubscriptionClient(
    'wss://' + splitUri[1],
    { reconnect: true }
  );
  return new WebSocketLink(subClient);
}

const initApollo = () => {
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
  const client = new ApolloClient({
    link,
    cache: new InMemoryCache({
      addTypename: false
    })
  })
  return client;
};

class App extends React.Component {
  state = {
    client: null,
  }

  async componentWillMount() {
    const client = await initApollo();
    this.setState({ client })
  }

  render() {
    if (!this.state.client) {
      return "Loading ...";
    }
    return (
      <ApolloProvider client={this.state.client}>
        <AppRouter />
      </ApolloProvider>
    );
  }
}

export default App;
