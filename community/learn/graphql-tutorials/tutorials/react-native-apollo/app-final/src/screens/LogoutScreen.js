import React from 'react';
import {
  AsyncStorage,
  View
} from 'react-native';
import { withApollo } from 'react-apollo';
import CenterSpinner from './components/Util/CenterSpinner';

class LogoutScreen extends React.Component {

  static navigationOptions = {
    drawerLabel: 'Logout',
    title: 'Logging out'
  };

  componentDidMount() {
    this.logout()
  }

  logout = () => {
    const { client } = this.props;
    AsyncStorage.removeItem('@todo-graphql:session').then(() => {
      client.resetStore();
      client.logout();
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

export default withApollo(LogoutScreen)

