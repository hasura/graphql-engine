import React from 'react';
import {
  StyleSheet,
  Text,
  TextInput,
  TouchableOpacity,
  View,
} from 'react-native';


export default class Textbox extends React.Component {

  state = {
    text: '',
  }

  render() {
    const { text } = this.state;
    const { isPublic } = this.props;
    const submit = () => {
      this.setState({
        text: ''
      });
    }
    return (
      <View style={styles.inputContainer}>
        <View style={styles.textboxContainer}>
          <TextInput
            style={styles.textbox}
            editable = {true}
            onChangeText = {this._handleTextChange}
            value = {text}
          />
        </View>
        <View style={styles.buttonContainer}>
          <TouchableOpacity style={styles.button} onPress={submit} disabled={text === ''}>
            <Text style={styles.buttonText}> Add </Text>
          </TouchableOpacity>
        </View>
      </View>
    );
  }

  _handleTextChange = (text) => {
    this.setState({
      text
    })
  }
}

const styles = StyleSheet.create({
  inputContainer: {
    flex: 0.1,
    padding: 10,
    flexDirection: 'row',
    justifyContent: 'space-between',
  },
  textbox: {
    flex: 1,
    padding: 10,
  },
  textboxContainer: {
    flex: 0.8,
    borderWidth: 0.5,
    paddingRight: 10,
    borderColor: '#d6d7da',
    borderRadius: 5,
  },
  buttonContainer: {
    flex: 0.2,
    paddingHorizontal: 5,
    paddingVertical: 2
  },
  button: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#39235A',
    borderColor: '#d6d7da',
    borderRadius: 5,
  },
  buttonText: {
    fontWeight: 'bold',
    color: 'white'
  }
});
