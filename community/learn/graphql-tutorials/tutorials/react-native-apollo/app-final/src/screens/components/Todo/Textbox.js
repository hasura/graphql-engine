import React from 'react';
import {
  StyleSheet,
  Text,
  TextInput,
  TouchableOpacity,
  View,
} from 'react-native';
import gql from 'graphql-tag';
import {Mutation} from 'react-apollo';
import { FETCH_TODOS } from './Todos';

const insertTodo = gql`
  mutation ($text: String!, $isPublic: Boolean){
    insert_todos (
      objects: [{
        title: $text,
        is_public: $isPublic
      }]
    ){
      returning {
        id
        title
        is_completed
        created_at
        is_public
        user {
          name
        }
      }
    }
  }
`;

export default class Textbox extends React.Component {

  state = {
    text: '',
  }

  render() {
    const { text } = this.state;
    const { isPublic } = this.props;
    return (
      <Mutation
        mutation={insertTodo}
        variables={{
          text,
          isPublic
        }}
        update={(cache, {data: {insert_todos}}) => {
          if (isPublic) { return; }
          const data = cache.readQuery({
            query: FETCH_TODOS,
            variables: {
              isPublic,
            }
          });
          const newTodo = insert_todos.returning[0];
          const newData = {
            todos: [ newTodo, ...data.todos]
          }
          cache.writeQuery({
            query: FETCH_TODOS,
            variables: {
              isPublic,
            },
            data: newData
          });
        }}
      >
        {
          (insertTodo, { loading, error}) => {
            const submit = () => {
              if (error) {
                console.log(error);
                return <Text> Error </Text>;
              }
              if (loading || text === '') {
                return;
              }
              this.setState({
                text: ''
              });
              insertTodo();
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
        }
        
      </Mutation>
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
