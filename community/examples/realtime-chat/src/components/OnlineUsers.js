import React from 'react';
import { Subscription } from 'react-apollo';
import moment from 'moment';
import gql from 'graphql-tag';

const fetchOnlineUsersSubscription = gql`
  subscription {
    user_online (
      order_by: username_asc
    ) {
      id
      username
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

  render() {
    return (
      <div className="onlineUsers">
        <Subscription
          subscription={fetchOnlineUsersSubscription}
        >
          {
            ({data, error, loading }) => {
              if (loading) {
                return null;
              }
              if (error) { return "Error loading online users"; }
              return (
                <div>
                 <p className="userListHeading"> Online Users ({!data.user_online ? 0 : data.user_online.length})</p>
                  <ul className="userList">
                    { 
                      data.user_online.map((u) => {
                        return <li key={u.id}>{u.username}</li>
                      })
                    }
                  </ul>
                </div>
              );
            }
          }
        </Subscription>
      </div>
    );
  }
};

export default OnlineUsers;
