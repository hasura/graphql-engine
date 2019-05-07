import { createStackNavigator, createDrawerNavigator, createAppContainer } from 'react-navigation';
import OnlineUsers from '../screens/UsersScreen';
import LogoutScreen from '../screens/LogoutScreen';
import TodosTabs from './TodosTabNavigator';

const LogoutStack = createStackNavigator({
  Logout: LogoutScreen
});

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
    screen: LogoutStack
  }
});

export default createAppContainer(Drawer);