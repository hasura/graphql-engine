import React from 'react';
import { Subscription } from 'react-apollo';
import moment from 'moment';
import gql from 'graphql-tag';
import UserList from './OnlineUsersList';

const fetchOnlineUsersSubscription = gql`
  subscription{
    user_online (
      limit: 1,
      order_by: last_seen_desc
    ){
      last_seen
    }
  }
`;

class OnlineUsers extends React.Component {
  
  constructor(props) {
    super(props);
    this.state = {
      time: moment().subtract(10, 'seconds').format(),
      refetch: null
    }
  }

  setRefetch = (refetch) => {
    this.setState({
      ...this.state,
      refetch
    });
  }

  render() {
    return (
      <div>
        <Subscription
          subscription={fetchOnlineUsersSubscription}
        >
          {
            ({data, error, loading }) => {
              if (loading) {
                return null;
              }
              if (error) { return "Error loading online users"; }
              if (this.state.refetch) {
                this.state.refetch();
              }
              return null;
            }
          }
        </Subscription>
        <UserList
          userId={this.props.userId}
          refetch={this.state.refetch}
          setRefetch={this.setRefetch}
        />
      </div>
    );
  }
};

export default OnlineUsers;
