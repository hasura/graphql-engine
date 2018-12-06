import React from 'react';
import {
  ScrollView,
  StyleSheet,
  Text,
  TouchableOpacity,
  View,
  FlatList,
} from 'react-native';
import TodoItem from './TodoItem';

export default class Todos extends React.Component {

  render() {
    const { isPublic } = this.props;
    const todoType = isPublic ? 'Public' : 'Private';
    const data = { ...dummyData};
    return (
      <View style={styles.container}>
        {isPublic && <NewTodosBanner fetch={this.fetchNewTodos} />}
        <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
          <FlatList
            data={data.todos}
            renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
            keyExtractor={(item) => item.id.toString()}
          />
          <LoadMoreButton updateCache={this.updateCache} fetchOlderTodos={this.fetchOlderTodos} />
        </ScrollView>
      </View>
    );
  }
}

const LoadMoreButton = () => (
  <TouchableOpacity
    style={styles.pagination}
    onPress={() => {
      console.log('Pressed');
    }}
  >
    {
      <Text style={styles.buttonText}> Load more todos </Text>
    }
  </TouchableOpacity>
);


const NewTodosBanner = () => (
  <TouchableOpacity
    onPress={() => console.log('pressed')}
    style={styles.banner}
  >
    <Text style={styles.buttonText}> Load new todos </Text>
  </TouchableOpacity>
)

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  scrollView: {
    flex: 0.8,
    paddingTop: 10,
    paddingHorizontal: 10,
    backgroundColor: '#F7F7F7'
  },
  scrollViewContainer: {
    justifyContent: 'flex-start'
  },
  banner: {
    flexDirection: 'column',
    backgroundColor: '#66BDE7',
    height: 40,
    justifyContent: 'center',
    alignItems: 'center',
    marginBottom: 5,
  },
  pagination: {
    flexDirection: 'column',
    backgroundColor: '#66BDE7',
    height: 40,
    justifyContent: 'center',
    alignItems: 'center',
    marginTop: 5,
    marginBottom: 20,
    paddingVertical: 5,
  },
  buttonText: {
    color: 'black'
  }
});

const dummyData = {
  todos: [
    {
      id: 1,
      text: "sample todo",
      is_completed: false,
      is_public: true,
      user: {
        name: "user 1"
      }
    },
    {
      id: 2,
      text: "sample todo",
      is_completed: false,
      is_public: true,
      user: {
        name: "user 1"
      }
    },
    {
      id: 3,
      text: "sample todo",
      is_completed: true,
      is_public: true,
      user: {
        name: "user 6"
      }
    },
    {
      id: 4,
      text: "sample todo",
      is_completed: false,
      is_public: true,
      user: {
        name: "user 4"
      }
    },
    {
      id: 5,
      text: "sample todo",
      is_completed: false,
      is_public: true,
      user: {
        name: "user 3"
      }
    },
    {
      id: 6,
      text: "sample todo",
      is_completed: true,
      is_public: true,
      user: {
        name: "user 1"
      }
    },
    {
      id: 7,
      text: "sample todo",
      is_completed: false,
      is_public: false,
      user: {
        name: "user 2"
      }
    },
    {
      id: 8,
      text: "sample todo",
      is_completed: true,
      is_public: false,
      user: {
        name: "user 3"
      }
    },
  ]
}
