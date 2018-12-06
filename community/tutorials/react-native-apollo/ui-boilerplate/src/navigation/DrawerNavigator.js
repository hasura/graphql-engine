import React from 'react';
import { createStackNavigator, createDrawerNavigator } from 'react-navigation';
import OnlineUsers from '../screens/UsersScreen';
import LogoutScreen from '../screens/LogoutScreen';
import TodosTabs from './MainTabNavigator';
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

export default class App extends React.Component {

  static router = Drawer.router;

  state = {
    loading: false
  }

  render() {
    if (this.state.loading) {
      return <CenterSpinner />
    }
    // provide Apollo client to the entire app using ApolloProvider
    return <Drawer screenProps={{rootNavigation: this.props.screenProps.rootNavigation}}/>
  }
}
