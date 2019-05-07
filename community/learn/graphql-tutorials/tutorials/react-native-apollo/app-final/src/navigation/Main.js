import React from 'react';
import { AsyncStorage } from 'react-native';
import Drawer from './DrawerNavigator';
import CenterSpinner from '../screens/components/CenterSpinner';
import { ApolloProvider} from 'react-apollo';
import gql from 'graphql-tag';
import makeApolloClient from '../apollo';

// GraphQL mutation to update last_seen
const emitOnlineEvent = gql`
mutation ($userId: String) {
  update_users(
    _set: {
      last_seen: "now()"
    },
    where: {}
  ) {
    affected_rows
  }
}
`;
export default class App extends React.Component {

  state = {
    client: null,
  }

  // bootstrap session in componentDidMount
  async componentDidMount() {
    // fetch session
    const session = await AsyncStorage.getItem('@todo-graphql:auth0');
    const sessionObj = JSON.parse(session);
    const { token, id } = sessionObj;
    // make apollo client with this session token
    const client = makeApolloClient(token);
    // start emitting events saying that the useri s online
    this.setState({ client });
    setInterval(
      () => client.mutate({
        mutation: emitOnlineEvent,
        variables: {
          userId: id
        }
      }),
      5000
    );
  }

  render() {
    if (!this.state.client) {
      return <CenterSpinner />
    }

    // provide Apollo client to the entire app using ApolloProvider
    return <ApolloProvider client={this.state.client}><Drawer/></ApolloProvider>
  }
}
