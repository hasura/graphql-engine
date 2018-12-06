import React from 'react';
import {
  ScrollView,
  StyleSheet,
  Text,
  TouchableOpacity,
  View,
  FlatList,
} from 'react-native';
import gql from 'graphql-tag';
import { Query } from 'react-apollo';
import handleError from '../../utils';
import TodoItem from './TodoItem';
import CenterSpinner from './CenterSpinner';

export const fetchTodos = gql`
query (
  $isPublic: Boolean,
){
  todos (
    order_by: id_desc,
    where: { is_public: { _eq: $isPublic} }
    limit: 10
  ) {
    id
    text
    is_completed
    created_at
    updated_at
    is_public
    user {
      name
    }
  }
}
`;

const subscribeToNewTodos = gql`
subscription {
  todos (
    order_by: id_desc
    limit: 1
    where: { is_public: { _eq: true }}
  ) {
    id
  }
}
`;

const fetchNewTodos = gql`
query ($lastId: Int){
  todos (
    order_by: id_desc,
    where: {
      _and: {
        is_public: { _eq: true},
        id: { _gt: $lastId}
      }
    }
  ) {
    id
    text
    is_completed
    created_at
    updated_at
    is_public
    user {
      name
    }
  }
}
`;

const fetchOldTodos = gql`
query ($lastId: Int, $isPublic: Boolean){
  todos (
    order_by: id_desc,
    where: {
      _and: {
        is_public: { _eq: $isPublic},
        id: { _lt: $lastId}
      }
    },
    limit: 10
  ) {
    id
    text
    is_completed
    created_at
    updated_at
    is_public
    user {
      name
    }
  }
}
`;
export default class Todos extends React.Component {

  constructor (props) {
    super(props);
    this.props.client.writeData({
      data: {
        newTodosExist: false,
        loadMoreButtonEnabledPublic: true, 
        loadMoreTextPublic: 'Load more todos',
        loadMoreButtonEnabledPrivate: true, 
        loadMoreTextPrivate: 'Load more todos'
      }
    });
  }

  updateCache = (key, value) => {
    const { client } = this.props;
    const resp = client.query({query: gql`{
      newTodosExist @client
      loadMoreButtonEnabledPublic @client
      loadMoreTextPublic @client
      loadMoreButtonEnabledPrivate @client
      loadMoreTextPrivate @client
    }`});
    const newData = {
      ...resp.data
    };
    newData[key] = value;
    client.writeData({
      data: {
        ...newData
      } 
    });
  }

  async componentDidMount() {
    const { client, isPublic } = this.props;
    if (isPublic) {
      client.subscribe({
        query: subscribeToNewTodos,
      }).subscribe({
        next: (event) => {
          if (event.data.todos.length) {
            let localData;
            try {
              localData = client.readQuery({
                query: fetchTodos,
                variables: {
                  isPublic: true,
                }
              });
            } catch (e) {
              return;
            } 
            
            const lastId = localData.todos[0] ? localData.todos[0].id : 0
            if (event.data.todos[0].id > lastId) {
              this.updateCache(`newTodosExist`, true)
            }
          }
        },
        error: (err) => {
          console.error("err", err);
        }
      })
    }
  }

  fetchNewTodos = async () => {
    const { client, isPublic } = this.props;
    const data = client.readQuery({
      query: fetchTodos,
      variables: {
        isPublic,
      }
    });
    const lastId = data.todos[0].id;
    const resp = await client.query({
      query: fetchNewTodos,
      variables: { lastId }
    });
    if (resp.data) {
      const newData = {
        todos: [ ...resp.data.todos, ...data.todos]
      }
      client.writeQuery({
        query: fetchTodos,
        variables: {
          isPublic,
        },
        data: newData
      });
      this.updateCache('newTodosExist', false)
    }
  }

  render() {
    const { isPublic } = this.props;
    const todoType = isPublic ? 'Public' : 'Private';
    return (
      <Query
        query={fetchTodos}
        variables={{isPublic: this.props.isPublic}}
      >
        {
          ({data, error, loading }) => {
            if (error) {
              handleError(error, this.props.navigate);
              return <Text>Error</Text>;
            }
            if (loading) {
              return <CenterSpinner />;
            }
            return (
              <View style={styles.container}>
                {isPublic && <NewTodosBanner fetch={this.fetchNewTodos} />}
                <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
                  <FlatList
                    data={data.todos}
                    renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
                    keyExtractor={(item) => item.id.toString()}
                  />
                  <LoadMoreButton updateCache={this.updateCache} fetchOlderTodos={this.fetchOlderTodos} todoType={todoType}/>
                </ScrollView>
              </View>
            );
          }
        }
      </Query>
    );
  }

  fetchOlderTodos = async () => {
    const { client, isPublic } = this.props;
    const todoType = isPublic ? 'Public' : 'Private';
    const data = client.readQuery({
      query: fetchTodos,
      variables: {
        isPublic,
      }
    });
    const response = await client.query({
      query: fetchOldTodos,
      variables: {
        isPublic,
        lastId: data.todos[data.todos.length - 1].id
      },
    });
    if (!response.data) {
      return;
    }
    if (response.data.todos) {
      client.writeQuery({
        query: fetchTodos,
        variables: {
          isPublic
        },
        data: { todos: [ ...data.todos, ...response.data.todos]}
      });
      if (response.data.todos < 10) {
        this.updateCache(`loadMoreText${todoType}`, 'No more todos');  
      } else {
        this.updateCache(`loadMoreButtonEnabled${todoType}`, true);
      }
    } else {
      this.updateCache(`loadMoreText${todoType}`, 'No more todos');  
    }
  }
}

const LoadMoreButton = ({updateCache, fetchOlderTodos, todoType}) => (
  <Query
    query={gql`{
      loadMoreButtonEnabledPublic @client
      loadMoreTextPublic @client
      loadMoreButtonEnabledPrivate @client
      loadMoreTextPrivate @client
    }`}
  >
    {
      ({data}) => {
        return (
          <TouchableOpacity
            style={styles.pagination}
            onPress={() => {
              updateCache(`loadMoreButtonEnabled${todoType}`, false);
              fetchOlderTodos();
            }}
            disabled={!data[`loadMoreButtonEnabled${todoType}`]}
          > 
            {
              !data[`loadMoreButtonEnabled${todoType}`] && data[`loadMoreText${todoType}`] !== 'No more todos' ?
              <CenterSpinner /> :
              <Text style={styles.buttonText}> {data[`loadMoreText${todoType}`]} </Text>
            }
          </TouchableOpacity> 
        )
      }
    }
  </Query>
);


const NewTodosBanner = (props) => (
  <Query
    query={gql`{ newTodosExist @client }`}
  >
    {
      ({data}) => {
        if (data && data.newTodosExist) {
          return (
            <TouchableOpacity
              onPress={props.fetch}
              style={styles.banner}
            >
              <Text style={styles.buttonText}> Load new todos </Text>
            </TouchableOpacity>
          )
        }
        return null;
      }
    }
  </Query>
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
});
