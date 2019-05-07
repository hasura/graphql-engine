import React from 'react';
import {
  AsyncStorage,
  View
} from 'react-native';
import { ApolloConsumer } from 'react-apollo';
import CenterSpinner from './components/CenterSpinner';

export default class LogoutScreen extends React.Component {
  static navigationOptions = {
    drawerLabel: 'Logout',
    title: 'Logging out'
  };

  componentDidMount() {
    this.logout();
  }

  logout = () => {
    AsyncStorage.removeItem('@todo-graphql:auth0').then(() => {
      this.props.navigation.navigate('Auth');
    });
  } 

  render() {
    return (
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center'}}>
        <CenterSpinner />
      </View>
    );
  }
}

