import React from 'react';
import TodoScreen from './TodoScreen';
import MenuButton from './components/Util/MenuButton';

export default class PublicTodos extends React.Component {
  static navigationOptions = ({ navigation }) => ({
    headerTitle: 'Public Todos',
    headerLeft: (
      <MenuButton onPress={navigation.toggleDrawer} /> 
    )
  });

  render() {
    // return TodoScreen with prop isPublic to true
    return (
      <TodoScreen isPublic={true} navigate={this.props.navigation.navigate}/> 
    );
  }
}
