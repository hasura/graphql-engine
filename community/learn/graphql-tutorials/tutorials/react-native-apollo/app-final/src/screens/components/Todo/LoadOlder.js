import React from 'react';
import { withApollo } from 'react-apollo';
import { TouchableOpacity, Text } from 'react-native'
import CenterSpinner from '../Util/CenterSpinner';
import { FETCH_TODOS } from './Todos';
import gql from 'graphql-tag';

const FETCH_OLD_TODOS = gql`
query ($lastId: Int, $isPublic: Boolean){
  todos (
    order_by: {
      id: desc
    },
    where: {
      _and: {
        is_public: { _eq: $isPublic},
        id: { _lt: $lastId}
      }
    },
    limit: 10
  ) {
    id
    title
    is_completed
    created_at
    is_public
    user {
      name
    }
  }
}
`

class LoadOlderButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'Load more todos',
      disabled: false,
      loading: false,
    };
  }

  fetchOlderTodos = async () => {
    const { client, isPublic } = this.props;
    const data = client.readQuery({
      query: FETCH_TODOS,
      variables: {
        isPublic,
      }
    });
    const numTodos = data.todos.length;
    this.setState({ disabled: true });
    this.setState({ loading: true });
    const response = await client.query({
      query: FETCH_OLD_TODOS,
      variables: {
        isPublic,
        lastId: numTodos === 0 ? 0 : data.todos[numTodos - 1].id
      },
    });
    this.setState({ loading: false });
    if (!response.data) {
      this.setState({ disabled: false })
      return;
    }
    if (response.data.todos) {
      client.writeQuery({
        query: FETCH_TODOS,
        variables: {
          isPublic
        },
        data: { todos: [ ...data.todos, ...response.data.todos]}
      });
      if (response.data.todos.length < 10) {
        this.setState({ buttonText: 'No more todos', disabled: true })
      } else {
        this.setState({ buttonText: 'Load more todos', disabled: false})
      }
    } else {
      this.setState({ buttonText: 'Load more todos' });  
    }
  }

  render () {
    const { disabled, buttonText, loading } = this.state;
    const { styles } = this.props;
    return (
      <TouchableOpacity
        style={styles.pagination}
        onPress={this.fetchOlderTodos}
        disabled={disabled}
      > 
        {
          loading ?
          <CenterSpinner /> :
          <Text style={styles.buttonText}>
            {buttonText}
          </Text>
        }
      </TouchableOpacity> 
    )
  }
}

export default withApollo(LoadOlderButton);