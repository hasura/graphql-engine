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
const auth0ClientId = 'lgKxyHzCDUWCALdAOkjg3QI2D6eglGes';
const auth0Domain = 'https://todo-hasura-test.auth0.com';


const toQueryString = (params) => {
  return '?' + Object.entries(params)
    .map(([key, value]) => `${encodeURIComponent(key)}=${encodeURIComponent(value)}`)
    .join('&');
};

export default class App extends React.Component {

  state = { 
    isLoggedIn: false 
  }

  loginWithAuth0 = async () => {
    // get redirect URL to redirect after log in
    const redirectUrl = AuthSession.getRedirectUrl();
    // perform login
    const result = await AuthSession.startAsync({
      authUrl: `${auth0Domain}/authorize` + toQueryString({
        client_id: auth0ClientId,
        response_type: 'token',
        scope: 'openid profile',
        redirect_uri: redirectUrl,
      }),
    });
    // if success, handle the result
    if (result.type === 'success') {
      this.handleParams(result.params);
    }
  }

  handleParams = (responseObj) => {
    // handle error
    if (responseObj.error) {
      Alert.alert('Error', responseObj.error_description
        || 'something went wrong while logging in');
      return;
    }
    // store session in storage and redirect back to the app
    const encodedToken = responseObj.id_token;
    const decodedToken = jwtDecoder(encodedToken);
    AsyncStorage.setItem(
      '@todo-graphql:auth0',
      JSON.stringify({
        token: encodedToken,
        name: decodedToken.nickname,
        id: decodedToken.sub,
        exp: decodedToken.exp
      })
    ).then(() => {
      this.setState({ isLoggedIn: true})
      this.props.navigation.navigate('Loading');
    })
  }

  render() {
    return (
      <View style={styles.container}>
        { this.state.isLoggedIn ?
          <CenterSpinner /> :
          <View>
            <Image
              source={{uri: "https://cdn.discordapp.com/attachments/483936123337572352/502036219220459520/Screen_Shot_2018-10-17_at_14.02.10.png"}}
              style={styles.image}
            />
            <TouchableOpacity
              style={styles.loginButton}
              onPress={this.loginWithAuth0}
            > 
              <Icon name={"login"} style={styles.buttonIcon} size={20}/>
              <Text style={styles.buttonText}> Login </Text>
            </TouchableOpacity>
          </View>
        }
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
  loginButton: {
    flexDirection: 'row',
    alignSelf: 'center',
    justifyContent: 'center',
    alignItems: 'center',
    height: 50,
    width: 250,
    marginVertical: 20,
    padding: 5,
    backgroundColor: '#F07D4C',
  },
  image: {
    height: 250,
    width: 400
  },
  buttonText: {
    fontWeight: 'bold',
    fontSize: 20
  },
  buttonIcon: {
  }
});