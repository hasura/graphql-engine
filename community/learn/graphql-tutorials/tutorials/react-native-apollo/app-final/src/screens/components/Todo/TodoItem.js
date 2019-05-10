import React from 'react';
import {
  Text,
  View,
  StyleSheet,
  TouchableOpacity,
  Alert
} from 'react-native';
import {Mutation} from 'react-apollo';
import gql from 'graphql-tag';
import Icon from 'react-native-vector-icons/MaterialIcons';
import { FETCH_TODOS } from './Todos';
import CenterSpinner from '../Util/CenterSpinner';

const UPDATE_TODO = gql`
  mutation ($id: Int, $isCompleted: Boolean) {
    update_todos (
      _set: {
        is_completed: $isCompleted
      },
      where: {
        id: {
          _eq: $id
        }
      }
    ) {
      returning {
        id
        title
        is_completed
        created_at
        is_public
        user {
          id
          name
        }
      }
    }
  }
`;

const DELETE_TODO = gql`
  mutation ($id: Int) {
    delete_todos (
      where: {
        id: {
          _eq: $id
        }
      }
    ) {
      affected_rows
    }
  }
`;

export default class TodoItem extends React.Component {

  render() {
    const { item, isPublic } = this.props;
    const userIcon = () => {
      if (!isPublic) {
        return null;
      }
      return (
        <TouchableOpacity
          style={styles.userItem}
          onPress={() => Alert.alert('Message', `This todo is by @${item.user.name}`)}
        >
          <Text style={styles.userText}>@{item.user.name.toLowerCase()}</Text>
        </TouchableOpacity>
      );
    }

    const updateCheckbox = () => {
      if (isPublic) return null;
      return (
        <Mutation
          mutation={UPDATE_TODO}
          variables={{
            id: item.id,
            isCompleted: !item.is_completed
          }}
        >
          {
            (updateTodo, {loading, error}) => {
              if (error) {
                console.log(error);
                return (<Text> Error </Text>);
              }
              const update = () => {
                if (loading) { return; }
                updateTodo();
              }
              return (
                <TouchableOpacity
                  style={item.is_completed ? styles.completedCheckBox : styles.checkBox}
                  onPress={update}
                  disabled={loading}
                >
                  { loading && <CenterSpinner />}
                </TouchableOpacity>
              )
            }
          }
        </Mutation>
      )
    }

    const todoText = () => (
      <View style={styles.todoTextWrapper}>
        <Text style={item.is_completed ? styles.completedText : styles.activeText}>
          {item.title}
        </Text>
      </View>
    )

    const deleteButton = () => {
      if (isPublic) return null;
      return (
        <Mutation
          mutation={DELETE_TODO}
          variables={{
            id: item.id,
          }}
          update={(cache) => {
            const data = cache.readQuery({
              query: FETCH_TODOS,
              variables: {
                isPublic,
              }
            });
            const newData = {
              todos: data.todos.filter((t) => t.id !== item.id)
            }
            cache.writeQuery({
              query: FETCH_TODOS,
              variables: {
                isPublic,
              },
              data: newData
            });
          }}
        >
          {
            (deleteTodo, { loading, error }) => {
              if (error) {
                return <Text> Error </Text>;
              }
              const remove = () => {
                if (loading) { return; }
                deleteTodo();
              };
              return (
                <View style={styles.deleteButton}>
                  <Icon
                    name="delete"
                    size={26}
                    onPress={remove}
                    disabled={loading}
                    color={loading ? "lightgray" : "#BC0000"}
                  />
                </View>
              );
            }
          }
        </Mutation> 
      )
    }

    const todoContainerStyle = isPublic ? styles.todoContainerPublic : styles.todoContainerPrivate;

    return (
      <View style={todoContainerStyle}>
        {userIcon()}
        {updateCheckbox()}
        {todoText()}
        {deleteButton()}
      </View>
    );
  }
}

const styles = StyleSheet.create({
  todoContainerPrivate: {
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
  todoContainerPublic: {
    margin: 5,
    padding: 5,
    flex: 1,
    flexDirection: 'row',
    justifyContent: 'flex-start',
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
    height: 30,
    backgroundColor: 'white',
    justifyContent: 'center',
    alignItems: 'center'
  },
  userText: {
    fontWeight: 'bold'
  },
});
