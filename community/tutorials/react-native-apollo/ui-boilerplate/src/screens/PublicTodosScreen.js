import React from 'react';
import TodoScreen from './TodoScreen';

export default class PublicTodos extends React.Component {
  static navigationOptions = {
    title: 'Public Todos'
  };

  render() {
    // return TodoScreen with prop isPublic to true
    return (
      <TodoScreen isPublic={true} navigate={this.props.navigation.navigate}/> 
    );
  }
}
