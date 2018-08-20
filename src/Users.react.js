import React from 'react';
import gql from 'graphql-tag';
import { Subscription } from 'react-apollo';
import {
  Well,
} from 'react-bootstrap';

const SUBSCRIPTION_ONLINE_USERS = `
  subscription {
    online_users {
      count
    }
  }
`

export const Users = () => (
  <Subscription subscription={gql`${SUBSCRIPTION_ONLINE_USERS}`}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div>
           <span>Online users: {data.online_users[0].count}</span>
           <Well><pre>{SUBSCRIPTION_ONLINE_USERS}</pre></Well>
         </div>
       );
    }}
  </Subscription>
)
