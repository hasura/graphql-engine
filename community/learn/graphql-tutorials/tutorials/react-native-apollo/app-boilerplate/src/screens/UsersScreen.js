import React from 'react';
import {
  Text,
  FlatList,
  StyleSheet,
  View,
  ScrollView
} from 'react-native';
import CenterSpinner from './components/CenterSpinner';
import MenuButton from '../components/MenuButton';

export default class OnlineUsers extends React.Component {

  static navigationOptions = ({ navigation }) => ({
    headerTitle: 'Online Users',
    headerLeft: (
      <MenuButton onPress={navigation.toggleDrawer} /> 
    )
  }); 

  render() {

    const data = {
      online_users: [
        {
          id: 1,
          last_seen: "now",
          user : {
            id: 1,
            name: "user1"
          }
        },
        {
          id: 2,
          last_seen: "now",
          user : {
            id: 2,
            name: "user2"
          }
        },
      ]
    };

    return (
      <View style={styles.container}>
        <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
        <FlatList
          data={data.online_users}
          renderItem={({item}) => <UserItem item={item} />}
          keyExtractor={(item) => item.user.name}
        />
        </ScrollView>
      </View>
    );
  }
}

const UserItem = ({item}) => (
  <View style={styles.userContainer}>
    <Text> {item.user.name} </Text>
    <View style={styles.greenDot} />
  </View>
)

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
  },
  scrollView: {
    flex: 0.8,
    paddingTop: 10,
    paddingHorizontal: 10
  },
  scrollViewContainer: {
    justifyContent: 'flex-start'
  },
  userContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    height: 50,
    borderRadius: 4,
    borderWidth: 0.5,
    borderColor: '#d6d7da',
    paddingLeft: 5,
    paddingRight: 10 
  },
  text: {
    fontSize: 12
  },
  greenDot: {
    backgroundColor: 'green',
    borderRadius: 20,
    height: 15,
    width: 15
  }
});