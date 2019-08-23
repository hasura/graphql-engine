import React from 'react';
import {
  AsyncStorage,
  View
} from 'react-native';
import CenterSpinner from './components/Util/CenterSpinner';
import { logout } from '../authActions';

class LogoutScreen extends React.Component {

  static navigationOptions = {
    drawerLabel: 'Logout',
    title: 'Logging out'
  };

  componentDidMount() {
    this.logout()
  }

  logout = () => {
    AsyncStorage.removeItem('@todo-graphql:session').then(() => {
      logout();
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

export default LogoutScreen

