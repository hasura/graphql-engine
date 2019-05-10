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
    const successcallback = () => {
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
          <View className={styles.logoWrapper}>
            <Image
              source={require('../images/hasura_logo_horizontal_blue.png')}
              style={styles.logo}
            />
          </View>
          <View style={styles.titleTextWrapper}>
            <Text style={styles.title}>React Native Todo App with GraphQL</Text>
          </View>
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
              <Text style={loginTabTextStyle}>LOG IN</Text>
            </TouchableOpacity>
            <TouchableOpacity style={signupTabStyle} onPress={() => this.switchTabs(1)}>
              <Text style={signupTabTextStyle}>SIGN UP</Text>
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
        <View
          style={styles.container}
        >
          {logo()}
          {tabs()}
        </View>
      </KeyboardAvoidingView>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'space-between',
    paddingBottom: 20
  },
  tabContainer: {
    flex: 0.5,
    width: 400
  },
  tabHeader: {
    flex: 0.3,
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    paddingHorizontal: 30,
    marginBottom: 40
  },
  tabContent: {
    flex: 1,
  },
  tab: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    borderBottomColor: '#D5CEE7',
    borderBottomWidth: 1,
  },
  activeTab: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    borderBottomColor: '#39235A',
    borderBottomWidth: 3,
  },
  tabHeaderText: {
    fontSize: 18,
    color: '#808389',
    alignSelf: 'center',
    marginBottom: 15
  },
  tabHeaderTextActive: {
    fontSize: 18,
    color: '#39235A',
    alignSelf: 'center',
    fontWeight: 'bold',
    marginBottom: 15
  },
  titleContainer: {
    flex: 0.5,
    marginTop: 40,
    paddingHorizontal: 30,
    justifyContent: 'space-between'
  },
  titleTextWrapper: {
    flex: 1,
    fontSize: 30,
    marginTop: 30
  },
  title: {
    fontSize: 50,
    textAlign: 'left',
    fontWeight: '900',
    color: '#39235A'
  },
  logoWrapper: {
    paddingHorizontal: 20,
    marginBottom: 40
  },
  logo: {
    height: 40,
    width: 134
  },
});
