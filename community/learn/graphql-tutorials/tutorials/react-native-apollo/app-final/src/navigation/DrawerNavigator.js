import { createStackNavigator, createDrawerNavigator, createAppContainer } from 'react-navigation';
import OnlineUsers from '../screens/UsersScreen';
import LogoutScreen from '../screens/LogoutScreen';
import TodosTabs from './TodosTabNavigator';

const UsersStack = createStackNavigator({
  Users: {
    screen: OnlineUsers,
    navigationOptions: () => ({ title: "Online Users" })
  }
});
// Drawer navigator
const Drawer = createDrawerNavigator({
  Todos: {
    screen: TodosTabs
  },
  Users: {
    screen: UsersStack
  },
  Logout: {
    screen: LogoutScreen
  }, 
}, {
  contentOptions: {
    activeTintColor: '#39235A',
    inactiveTintColor: 'black',
    inactiveBackgroundColor: 'transparent',
    labelStyle: {
      fontSize: 15,
      marginLeft: 10,
    },
  },
});

const DrawerContainer = createAppContainer(Drawer);

export default DrawerContainer