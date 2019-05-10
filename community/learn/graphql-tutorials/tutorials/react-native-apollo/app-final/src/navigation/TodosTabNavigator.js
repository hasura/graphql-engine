import React from 'react';
import { createStackNavigator, createBottomTabNavigator, createAppContainer } from 'react-navigation';
import TabBarIcon from './components/TabBarIcon';
import PrivateTodos from '../screens/PrivateTodosScreen';
import PublicTodos from '../screens/PublicTodosScreen';

const PrivateTodosStack = createStackNavigator({
  Private: PrivateTodos,
});

PrivateTodosStack.navigationOptions = {
  tabBarLabel: 'Private Todos',
  tabBarIcon: ({ focused }) => (
    <TabBarIcon
      focused={focused}
      name="lock-outline"
    />
  ),
  tabBarOptions: {
    activeTintColor: '#392F76',
    inactiveTitColor: 'gray'
  }
};

const PublicTodosStack = createStackNavigator({
  Public: PublicTodos,
});

PublicTodosStack.navigationOptions = {
  tabBarLabel: 'Public Todos',
  tabBarIcon: ({ focused }) => (
    <TabBarIcon
      focused={focused}
      name="public"
    />
  ),
  tabBarOptions: {
    activeTintColor: '#392F76',
    inactiveTitColor: 'gray'
  }
};

const TodosTabNavigator = createBottomTabNavigator({
  PrivateTodosStack,
  PublicTodosStack,
});

export default createAppContainer(TodosTabNavigator);