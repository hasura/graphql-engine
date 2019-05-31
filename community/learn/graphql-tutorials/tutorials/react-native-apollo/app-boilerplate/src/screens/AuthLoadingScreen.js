import React from 'react';
import {
  AsyncStorage,
  View,
} from 'react-native';
import CenterSpinner from './components/Util/CenterSpinner';
import {setLogout} from '../authActions';

export default class AuthLoadingScreen extends React.Component {
  constructor(props) {
    super(props);
  }

  async componentDidMount () {
    // Find session in storage and redirect accordingly
    await this._bootstrapAsync();
  }

  async componentDidUpdate() {
    await this._bootstrapAsync();
  }

  _bootstrapAsync = async () => {
    // Fetch token from storage
    const session = await AsyncStorage.getItem('@todo-graphql:session');
    // If session exists, validate it, else redirect to login screen
    if (session) {
      const sessionObj = JSON.parse(session);
      var currentTime = Math.floor(new Date().getTime() / 1000);
      if (currentTime < sessionObj.exp) {
        setLogout(() => this.props.navigation.navigate('Auth'));
        this.props.navigation.navigate('Main');
      } else {
        this.props.navigation.navigate('Auth');
      }
    } else {
      this.props.navigation.navigate('Auth');
    }
  };

  render() {
    return (
      <View>
        <CenterSpinner />
      </View>
    );
  }
}

