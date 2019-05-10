import React, { Component, Fragment } from 'react';
import {withApollo, Subscription} from 'react-apollo';
import gql from 'graphql-tag';

import OnlineUser from './OnlineUser';

class OnlineUsersWrapper extends Component {
  constructor(props) {
    super(props);
    this.client = props.client;
  }

  updateLastSeen() {
    // Use the apollo client to run a mutation to update the last_seen value
    const UPDATE_LASTSEEN_MUTATION=gql`
      mutation updateLastSeen ($now: timestamptz!) {
        update_users(where: {}, _set: {last_seen: $now}) {
          affected_rows
        }
      }`;
    this.client.mutate({
      mutation: UPDATE_LASTSEEN_MUTATION,
      variables: {now: (new Date()).toISOString()}
    });
  }

  componentDidMount() {
    // Every 30s, run a mutation to tell the backend that you're online
    this.updateLastSeen();
    this.onlineIndicator = setInterval(() => this.updateLastSeen(), 30000);
  }

  componentWillUnmount() {
    // Clean up
    clearInterval(this.onlineIndicator);
  }

  render() {
    return (
      <div className="onlineUsersWrapper">
        <Subscription subscription={gql`
          subscription getOnlineUsers {
            online_users(order_by: {user: {name: asc }}) {
              id
              user {
                name
              }
            }
          }`}>
          {({ loading, error, data }) => {
            if (loading) {
              return (<span>Loading...</span>);
            }
            if (error) {
              console.error(error);
              return (<span>Error!</span>);
            }
            if (data) {
              const users = data.online_users;
              const onlineUsersList = [];
              users.forEach((u, index) => {
                onlineUsersList.push(
                  <OnlineUser
                    key={index}
                    index={index}
                    user={u.user}
                  />);
              });
              return (
                <Fragment>
                  <div className="sliderHeader">
                    Online users - {users.length}
                  </div>
                  {onlineUsersList}
                </Fragment>
              );
            }
          }}
        </Subscription>
      </div>
    );
  }
}

export default withApollo(OnlineUsersWrapper);
