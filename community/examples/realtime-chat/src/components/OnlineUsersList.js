import React from 'react';
import { Query } from 'react-apollo';
import moment from 'moment';
import gql from 'graphql-tag';

const fetchOnlineUsersQuery = gql`
  query ($timestamp: timestamptz!, $selfId: Int ) {
    user (
      where: {
        online : {
          last_seen: {
            _gt: $timestamp
          }
        }
      },
      order_by: username_asc
    ){
      id
      username
    }
  }
`;

class UserList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      time: moment().subtract(10, 'seconds').format(),
      refetch: null
    }
  }

  getQueryVariables() {
    return {
      timestamp: moment().subtract(10, 'seconds').format(),
      selfId: this.props.userId
    };
  }

  refetchOnlineUsers = async () => {
    if (this.state.refetch) {
      const resp = await this.state.refetch(this.getQueryVariables())
      if (resp.data) {
        this.setState({
          ...this.state,
          users: [ ...resp.data.user]
        });
      }
    }
  }

  render() {
    const { users } = this.state;
    if (this.state.refetch && !this.props.refetch) {
      this.props.setRefetch(this.refetchOnlineUsers);
    }
    return (
      <div className="onlineUsers">
        <p className="userListHeading"> Online Users ({!users ? 0 : users.length})</p>
        <Query
          query={fetchOnlineUsersQuery}
          variables={this.getQueryVariables()}
        >
          {
            ({data, error, loading, refetch}) => {
              if (loading) {
                return null;
              }
              if (error) { return "Error loading online users"; }
              if (!this.state.refetch) {
                this.setState({
                  ...this.state,
                  refetch
                });
              }
              return null;
            }
          }
        </Query>
        <div>
          <ul className="userList">
            { users ?
              users.map((u) => {
                return <li key={u.id}>{u.username}</li>
              }) :
              null
            }
          </ul>
        </div>
      </div> 
    ) 
  }
}

export default UserList