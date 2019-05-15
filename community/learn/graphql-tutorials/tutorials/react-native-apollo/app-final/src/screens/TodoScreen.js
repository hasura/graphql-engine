import React from 'react';
import {
  StyleSheet,
  View,
  AsyncStorage,
  ActivityIndicator,
} from 'react-native';
import Textbox from './components/Todo/Textbox';
import Todos from './components/Todo/Todos';
import {ApolloConsumer} from 'react-apollo';

export default class TodoScreen extends React.Component {

  state = {
    id: null,
    token: null,
    name: null
  }

  async componentDidMount() {
    const session = await AsyncStorage.getItem('@todo-graphql:session');
    const {id, name, token} = JSON.parse(session);
    // set session details in state
    this.setState({
      id,
      name,
      token
    })
  }

  render() {
    if (!this.state.token) {
      return <ActivityIndicator />
    }
    // provide session details to children components
    return (
      <View style={styles.container}>
        <Textbox
          isPublic={this.props.isPublic}
          navigate={this.props.navigate}
          userId={this.state.id}
          username={this.state.name}
          token={this.state.token}
        />
        <ApolloConsumer>
          {
            client => (
              <View
                style={styles.todoListContainer}
              >
                <Todos
                  isPublic={this.props.isPublic}
                  navigate={this.props.navigate}
                  userId={this.state.id}
                  username={this.state.name}
                  token={this.state.token}
                  client={client}
                />
              </View>
            )
          }

        </ApolloConsumer>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    padding: 10,
    justifyContent: 'flex-start'
  },
  todoListContainer: {
    flex: 1,
    borderRadius: 5,
    borderWidth: 0.5,
    borderColor: '#d6d7da',
  }
});
