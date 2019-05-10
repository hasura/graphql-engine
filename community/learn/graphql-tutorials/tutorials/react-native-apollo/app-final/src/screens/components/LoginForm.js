import React from 'react';
import {
  View,
  Text,
  TextInput,
  TouchableOpacity,
  StyleSheet,
  Alert
} from 'react-native';
import CenterSpinner from './CenterSpinner';
import Icon from 'react-native-vector-icons/MaterialIcons';

const emailRegex = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/; 

class LoginForm extends React.Component {

  state = {
    email: this.props.email || '',
    password: this.props.password || '',
    loading: false
  }

  handleEmailChange = (text) => {
    this.setState({ email: text });
  }

  handlePasswordChange = (text) => {
    this.setState({ password: text });
  }

  handleSubmit = () => {
    const { email, password } = this.state;
    if (!emailRegex.test(email.toLowerCase())) {
      Alert.alert('Invalid email', 'Please enter a valid email address');
      return;
    }
    if (!email || !password) {
      Alert.alert('Email or password cannot be empty');
      return;
    }
    const successCallback = () => {
      if (this.props.type === 'login') {
        this.setState({
          email: '',
          password: '',
        })
      }
      this.setState({
        loading: false
      })
    };
    const errorCallback = (e) => {
      this.setState({
        loading: false
      });
      Alert.alert(e.title, e.message);
    }
    this.setState({loading: true})
    this.props.submit(email.toLowerCase(), password, successCallback, errorCallback)
  }

  render(){
    const { email, password, loading } = this.state;
    const buttonText = this.props.type === 'signup' ? 'SIGN UP' : 'LOG IN';
    return (
      <View style={styles.container}>
        <View style={styles.textboxWrapper}>
          <View style={styles.labelWrapper}>
            <Icon
              name="mail-outline"
              size={14}
              style={styles.labelIcon}
            />
            <Text style={styles.labelText}> Email </Text>
          </View>
          <TextInput
            style={styles.textbox}
            placeholder="Email"
            placeholderTextColor="#808389"
            type="email"
            value={email}
            onChangeText={this.handleEmailChange}
          />
        </View>
        <View style={styles.textboxWrapper}>
          <View style={styles.labelWrapper}>
            <Icon
              name="lock-outline"
              size={13}
              style={styles.labelIcon}
            />
            <Text style={styles.labelText}> Password </Text>
          </View>
          <TextInput
            style={styles.textbox}
            placeholder="Password"
            placeholderTextColor="#808389"
            secureTextEntry
            value={password}
            onChangeText={this.handlePasswordChange}
          />
        </View>
        <TouchableOpacity
          style={styles.buttonWrapper}
          onPress={this.handleSubmit}
          disabled={loading}
        >
          {
            loading ?
            <CenterSpinner />
            :
            <Text style={{color: 'white', fontWeight: 'bold'}} >{buttonText}</Text>
          }
        </TouchableOpacity>
      </View>
    )
  }
}

export default LoginForm;

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    paddingBottom: 20
  },
  textboxWrapper: {
    height: 60,
    width: 340,
    fontSize: 50,
    marginBottom: 20
  },
  textbox: {
    backgroundColor: '#D5CEE7',
    borderRadius: 7,
    padding: 5,
    paddingHorizontal: 10,
    fontSize: 13,
    height:40,
  },
  buttonWrapper: {
    flexDirection: 'row',
    justifyContent: 'center',
    alignItems: 'center',
    height: 50,
    width: 340,
    backgroundColor: '#39235A',
    marginBottom: 30,
    borderRadius: 20
  },
  labelWrapper: {
    flexDirection: 'row',
    marginBottom: 5,
    alignItems: 'center'
  },
  labelText: {
    fontSize: 13,
  },
  labelIcon: {
    marginRight: 5
  }
})