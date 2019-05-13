import React from 'react';
import TodoScreen from './TodoScreen';
import MenuButton from './components/Util/MenuButton';

export default class PrivateTodos extends React.Component {

  static navigationOptions = ({ navigation }) => ({
    headerTitle: 'Private Todos',
    headerLeft: (
      <MenuButton onPress={navigation.toggleDrawer} /> 
    )
  });

  render() {
    // return TodoScreen with prop isPublic as false
    return (
      <TodoScreen isPublic={false}/> 
    );
  }
}
