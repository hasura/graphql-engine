import React from 'react';
import {
  AsyncStorage,
  View
} from 'react-native';
import CenterSpinner from './components/CenterSpinner';

export default class LogoutScreen extends React.Component {
  static navigationOptions = {
    drawerLabel: 'Logout',
    title: 'Logging out'
  };

  logout = () => {
    AsyncStorage.removeItem('@todo-graphql:auth0').then(() => {
      this.props.screenProps.rootNavigation.navigate('Loading');
    })
  } 

  render() {
    return (
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center'}}>
        <CenterSpinner />
      </View>
    );
  }
}

