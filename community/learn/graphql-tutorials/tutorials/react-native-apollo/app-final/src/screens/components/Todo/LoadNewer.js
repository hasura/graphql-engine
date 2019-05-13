import React from 'react';
import { withApollo } from 'react-apollo';
import { TouchableOpacity, Text } from 'react-native'
import CenterSpinner from '../Util/CenterSpinner';
import { FETCH_TODOS } from './Todos';
import gql from 'graphql-tag';

const FETCH_NEW_TODOS = gql`
query ($lastId: Int){
  todos (
    order_by: {
      id: desc
    },
    where: {
      _and: {
        is_public: { _eq: true},
        id: { _gt: $lastId}
      }
    }
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
`;

class LoadNewerButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'New tasks have arrived!',
      loading: false,
    };
  }

  fetchNewerTodos = async () => {
    const { client, isPublic } = this.props;
    const data = client.readQuery({
      query: FETCH_TODOS,
      variables: {
        isPublic,
      }
    });
    const lastId = data.todos[0].id;
    this.setState({ loading: true})
    const resp = await client.query({
      query: FETCH_NEW_TODOS,
      variables: { lastId }
    });
    this.setState({ loading: false})
    if (resp.data) {
      const newData = {
        todos: [ ...resp.data.todos, ...data.todos]
      }
      client.writeQuery({
        query: FETCH_TODOS,
        variables: {
          isPublic,
        },
        data: newData
      });
      this.props.toggleShow();
    }
  }

  render () {
    const { disabled, buttonText, loading } = this.state;
    const { styles, show } = this.props;
    if (!show) {
      return null;
    }
    return (
      <TouchableOpacity
        style={styles.banner}
        onPress={this.fetchNewerTodos}
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

export default withApollo(LoadNewerButton);