import { AuthSession } from 'expo';
import React from 'react';
import {
  Alert,
  StyleSheet,
  Text,
  View,
  AsyncStorage,
  TouchableOpacity,
  Image
} from 'react-native';
import jwtDecoder from 'jwt-decode';
import Icon from 'react-native-vector-icons/Entypo';
import CenterSpinner from './components/CenterSpinner';
import Login from './components/Login';
import Signup from './components/Signup';

export default class App extends React.Component {

  state = {
    isLoggedIn: false,

  }

  render() {
    return (
      <View style={styles.container}>
        <Login />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    paddingTop: 50,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'space-around',
  },
});
