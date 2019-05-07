import React from 'react';
import {
  ScrollView,
  StyleSheet,
  Text,
  View,
  FlatList,
} from 'react-native';
import handleError from '../../utils';
import TodoItem from './TodoItem';
import LoadOlder from './LoadOlder';
import LoadNewer from './LoadNewer';
import CenterSpinner from './CenterSpinner';

export default class Todos extends React.Component {

  constructor (props) {
    super(props);
  }

  render() {
    const { isPublic } = this.props;
    const data = {
      todos: [
        {
          id: "1",
          title: "This is todo 1",
          is_completed: true,
          is_public: isPublic,
          user: {
            id: "1",
            name: "user1"
          }
        },
        {
          id: "2",
          title: "This is todo 2",
          is_completed: false,
          is_public: isPublic,
          user: {
            id: "2",
            name: "user2"
          }
        }
      ]
    }
    return (
      <View style={styles.container}>
        <LoadNewer show={isPublic} toggleShow={this.dismissNewTodoBanner} styles={styles} isPublic={this.props.isPublic}/>
        <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
          <FlatList
            data={data.todos}
            renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
            keyExtractor={(item) => item.id.toString()}
          />
          <LoadOlder
            isPublic={this.props.isPublic}
            styles={styles}
          />
        </ScrollView>
      </View>
    );
  }
}

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
});
