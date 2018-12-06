import React from 'react';
import TodoScreen from './TodoScreen';

export default class PrivateTodos extends React.Component {
  static navigationOptions = {
    title: 'Private Todos'
  };

  render() {
    // return TodoScreen with prop isPublic as false
    return (
      <TodoScreen isPublic={false} navigate={this.props.navigation.navigate}/> 
    );
  }
}
