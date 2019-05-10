import { AuthSession } from 'expo';
import React from 'react';
import {
  Alert,
  StyleSheet,
  Text,
  View,
  AsyncStorage,
  TouchableOpacity,
  Image,
  KeyboardAvoidingView,
  ScrollView,
} from 'react-native';
import jwtDecoder from 'jwt-decode';
import Icon from 'react-native-vector-icons/Entypo';
import CenterSpinner from './components/CenterSpinner';
import Login from './components/Login';
import Signup from './components/Signup';
import logo from '../images/learn-hasura.png';
import { LinearGradient } from 'expo';
import { signup, login } from '../authActions';

export default class Auth extends React.Component {

  state = {
    isLoggedIn: false,
    tabIndex: 0,
    loginProps: {
      email: '',
      password: ''
    }
  }

  switchTabs = (tabIndex) => {
    this.setState({
      tabIndex
    });
  }

  performSignup = (email, password, successCb, errorCb) => {
    const successCallback = () => {
      successCb();
      this.setState({
        loginProps: { email, password },
        tabIndex: 0
      });
      Alert.alert('Success!', 'Successfully signed up! Please login.')
    }
    signup(email, password, successCallback, errorCb);
  }

  performLogin = (email, password, successCb, errorCb) => {
    const successCallback = (response) => {
      successCb();
      this.setState({
        tabIndex: 0
      });
      const decodedToken = jwtDecoder(response.token);
      AsyncStorage.setItem('@todo-graphql:session', JSON.stringify({
        token: response.token,
        name: decodedToken.name,
        id: decodedToken.sub,
        exp: decodedToken.exp
      })).then(() => {
        this.props.navigation.navigate('Loading');
      })
    }
    login(email, password, successCallback, errorCb);
  }

  render() {

    const { tabIndex, loginProps } = this.state;

    const logo = () => {
      return (
        <View style={styles.titleContainer}>
          <Text style={styles.title}>React Native Todo App with GraphQL</Text>
        </View>
      )
    }

    const tabs = () => {
      const displayTab = tabIndex === 0 ? <Login submit={this.performLogin} {...loginProps}/> : <Signup submit={this.performSignup}/>
      let loginTabStyle, signupTabStyle, loginTabTextStyle, signupTabTextStyle;
      if (tabIndex === 0) {
        loginTabStyle = styles.activeTab;
        signupTabStyle = styles.tab;
        loginTabTextStyle = styles.tabHeaderTextActive;
        signupTabTextStyle = styles.tabHeaderText
      } else {
        signupTabStyle = styles.activeTab;
        loginTabStyle = styles.tab;
        loginTabTextStyle = styles.tabHeaderText;
        signupTabTextStyle = styles.tabHeaderTextActive;
      }

      return (
        <View
          style={styles.tabContainer}
        >
          <View style={styles.tabHeader}>
            <TouchableOpacity style={loginTabStyle} onPress={() => this.switchTabs(0)}>
              <Text style={loginTabTextStyle}>Login</Text>
            </TouchableOpacity>
            <TouchableOpacity style={signupTabStyle} onPress={() => this.switchTabs(1)}>
              <Text style={signupTabTextStyle}>Signup</Text>
            </TouchableOpacity>
          </View>
          <View
            style={styles.tabContent}
          >
            {displayTab}
          </View>
        </View>
      )
    }

    return (
      <KeyboardAvoidingView
        style={{flex: 1}}
        behaviour="height"
        keyboardVerticalOffset={50}
        enabled
      >
        <LinearGradient
          colors={['#392F76', '#391E4C']}
          style={styles.container}
        >
          {logo()}
          {tabs()}
        </LinearGradient>
      </KeyboardAvoidingView>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'flex-end',
    paddingBottom: 20
  },
  tabContainer: {
    flex: 0.5,
    borderColor: '#bbb',
    borderWidth: 1,
    borderRadius: 20,
    width: 400
  },
  tabHeader: {
    flex: 0.3,
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
  },
  tabContent: {
    flex: 1,
  },
  tab: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  activeTab: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    borderBottomColor: '#bbb',
    borderBottomWidth: 1,
  },
  tabHeaderText: {
    fontSize: 18,
    color: '#808389',
    alignSelf: 'center',
    marginBottom: 15
  },
  tabHeaderTextActive: {
    fontSize: 18,
    color: 'white',
    alignSelf: 'center',
    marginBottom: 15
  },
  titleContainer: {
    fontSize: 30,
    marginBottom: 40
  },
  title: {
    fontSize: 30,
    textAlign: 'center',
    color: 'white'
  }
});
