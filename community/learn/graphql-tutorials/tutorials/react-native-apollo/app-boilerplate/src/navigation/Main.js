import React from 'react';
import { AsyncStorage } from 'react-native';
import Drawer from './DrawerNavigator';
import CenterSpinner from '../screens/components/Util/CenterSpinner';

console.disableYellowBox = true;

export default class App extends React.Component {

  // bootstrap session in componentDidMount
  async componentDidMount() {
    // fetch session
    const session = await AsyncStorage.getItem('@todo-graphql:session');
    const sessionObj = JSON.parse(session);
    const { token, id } = sessionObj;
  }

  render() {
    // provide Apollo client to the entire app using ApolloProvider
    return <Drawer />
  }
}
