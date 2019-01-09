import React from 'react';
import {
  Text,
  View,
  StyleSheet,
  TouchableOpacity,
  Alert
} from 'react-native';
import Icon from 'react-native-vector-icons/MaterialIcons';

export default class TodoItem extends React.Component {

  render() {
    const { item, isPublic } = this.props;
    return (
      <View style={styles.todoContainer}>
        { isPublic && <UserItem username={item.user.name}/>}
        <TouchableOpacity
          style={item.is_completed ? styles.completedCheckBox : styles.checkBox}
          onPress={() => Alert.alert('Pressed')}
        >
        </TouchableOpacity>
        <View style={styles.todoTextWrapper}>
          <Text style={item.is_completed ? styles.completedText : styles.activeText}>
            {item.text}
          </Text>
        </View>
        <View style={styles.deleteButton}>
          <Icon
            name="delete"
            size={26}
            onPress={() => Alert.alert('Pressed')}
            color="#BC0000"
          />
        </View>
      </View>
    );
  }
}

const UserItem = ({username}) => (
  <TouchableOpacity
    style={styles.userItem}
    onPress={() => Alert.alert('Message', `This todo is by ${username}`)}
  >
    <Text style={styles.userText}>{username[0].toUpperCase()}</Text>
  </TouchableOpacity>
);

const styles = StyleSheet.create({
  todoContainer: {
    margin: 5,
    padding: 5,
    flex: 1,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    minHeight: 40,
    borderRadius: 4,
    borderWidth: 0.5,
    borderColor: '#d6d7da',
    backgroundColor: 'white'
  },
  todoTextWrapper: {
    flex: 0.7,
    margin: 5,
  },
  completedText: {
    textDecorationLine: 'line-through',
    textDecorationStyle: 'solid',
    flexWrap: 'wrap'
  },
  activeText: {
    flexWrap: 'wrap'
  },
  deleteButton: {
    flex: 0.1
  },
  checkBox: {
    flex: 0.1,
    height: 30,
    borderRadius: 20,
    borderWidth: 0.5,
    borderColor: '#d6d7da',
    backgroundColor: 'white'
  },
  completedCheckBox: {
    flex: 0.1,
    height: 30,
    borderRadius: 20,
    borderWidth: 0.5,
    borderColor: '#d6d7da',
    backgroundColor: 'green'
  },
  userItem: {
    flex: 0.1,
    height: 30,
    backgroundColor: 'white',
    justifyContent: 'center',
    alignItems: 'center'
  },
  userText: {
    fontWeight: 'bold'
  },
});
