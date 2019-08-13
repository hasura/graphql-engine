import React, { useEffect, Fragment, useState } from 'react';
import { useMutation, useSubscription } from '@apollo/react-hooks';
import gql from 'graphql-tag';

import OnlineUser from './OnlineUser';

const OnlineUsersWrapper = () => {
    // this.client = props.client;
    const [onlineIndicator, setOnlineIndicator] = useState(0);
    let users, onlineUsersList; 

  useEffect(() => {
    // Every 30s, run a mutation to tell the backend that you're online
    updateLastSeen();
    setOnlineIndicator(setInterval(() => updateLastSeen(), 30000));

    return () => {
       // Clean up
      clearInterval(onlineIndicator);
    }
  }, [])  
  
  const UPDATE_LASTSEEN_MUTATION=gql`
      mutation updateLastSeen ($now: timestamptz!) {
        update_users(where: {}, _set: {last_seen: $now}) {
          affected_rows
        }
      }`;
  const [updateLastSeenMutation] = useMutation(UPDATE_LASTSEEN_MUTATION);

  const updateLastSeen = () => {
    // Use the apollo client to run a mutation to update the last_seen value
    updateLastSeenMutation({
      variables: {now: (new Date()).toISOString()}
    });
  }

  const { loading, error, data } = useSubscription(
    gql`
          subscription getOnlineUsers {
            online_users(order_by: {user: {name: asc }}) {
              id
              user {
                name
              }
            }
          }`
  );

  if (loading) {
    return (<span>Loading...</span>);
  }
  if (error) {
    console.error(error);
    return (<span>Error!</span>);
  }
  if (data) {
    users = data.online_users;
    onlineUsersList = [];
    users.forEach((u, index) => {
      onlineUsersList.push(
        <OnlineUser
          key={index}
          index={index}
          user={u.user}
        />);
    });
  }

    return (
          <div className="onlineUsersWrapper">
                <Fragment>
                  <div className="sliderHeader">
                    Online users - {users.length}
                  </div>
                  {onlineUsersList}
                </Fragment>
          </div>
    );
  }

export default OnlineUsersWrapper;
