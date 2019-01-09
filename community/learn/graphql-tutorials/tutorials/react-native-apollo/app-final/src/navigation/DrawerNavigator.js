import React from 'react';
import {
  AsyncStorage
} from 'react-native';
import { createStackNavigator, createDrawerNavigator } from 'react-navigation';
import OnlineUsers from '../screens/UsersScreen';
import LogoutScreen from '../screens/LogoutScreen';
import TodosTabs from './MainTabNavigator';
import { ApolloProvider} from 'react-apollo';
import makeApolloClient from '../apollo';
import gql from 'graphql-tag';
import CenterSpinner from '../screens/components/CenterSpinner';

const LogoutStack = createStackNavigator({
  Logout: (props) => <LogoutScreen screenProps={{rootNavigation: props.screenProps.rootNavigation}}/>
});

const UsersStack = createStackNavigator({
  Users: {
    screen: (props) => <OnlineUsers screenProps={{rootNavigation: props.screenProps.rootNavigation}}/>,
    navigationOptions: () => ({ title: "Online Users" })
  }
});
// Drawer navigator
const Drawer = createDrawerNavigator({
  Todos: {
    screen: (props) => <TodosTabs screenProps={{rootNavigation: props.screenProps.rootNavigation}} />
  },
  Users: {
    screen: (props) => <UsersStack screenProps={{rootNavigation: props.screenProps.rootNavigation}} />
  },
  Logout: {
    screen: (props) => <LogoutStack screenProps={{rootNavigation: props.screenProps.rootNavigation}} />
  }
});

// GraphQL Mutation to upsert user on login
const insertUser = gql`
  mutation ($auth0_id: String!, $name: String!) {
      insert_users (
      objects: [
        {
          name: $name,
          auth0_id:$auth0_id
        }
      ],
      on_conflict: {
        constraint: users_pkey,
        update_columns: [auth0_id, name]
      }
    ) {
      affected_rows
    }
  }
`;

// GraphQL mutation to update last_seen
const emitOnlineEvent = gql`
mutation ($userId: String) {
  update_users(
    _set: {
      last_seen: "now()"
    },
    where: {
      auth0_id: {
        _eq: $userId
      }
    }
  ) {
    affected_rows
  }
}
`
export default class App extends React.Component {

  static router = Drawer.router;

  state = {
    client: null,
    loading: true
  }

  // bootstrap session in componentDidMount
  async componentDidMount() {
    // fetch session
    const session = await AsyncStorage.getItem('@todo-graphql:auth0');
    const sessionObj = JSON.parse(session);
    const { token, id, name } = sessionObj;
    // make apollo client with this session token
    const client = makeApolloClient(token);
    // insert the user into user table
    const resp = await client.mutate({
      mutation: insertUser,
      variables: {
        auth0_id: id,
        name
      }
    });
    // set loading to false
    if (resp.data) {
      this.setState({
        client,
        loading: false
      })
    }

    // start emitting events saying that the useri s online
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
    if (this.state.loading) {
      return <CenterSpinner />
    }
    // provide Apollo client to the entire app using ApolloProvider
    return <ApolloProvider client={this.state.client}><Drawer screenProps={{rootNavigation: this.props.screenProps.rootNavigation}}/></ApolloProvider>
  }
}
