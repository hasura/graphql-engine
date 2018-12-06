import React from 'react';
import { createSwitchNavigator } from 'react-navigation';
import AuthLoadingScreen from '../screens/AuthLoadingScreen';
import AuthScreen from '../screens/AuthScreen';
import App from './DrawerNavigator';

export default createSwitchNavigator(
  {
    // For authentication
    Auth: AuthScreen,
    // For fetching and validating session
    Loading: AuthLoadingScreen,
    // Main app
    Main: ({navigation}) => <App screenProps={{rootNavigation: navigation}} />
  },
  {
    initialRouteName: 'Loading'
  } 
);