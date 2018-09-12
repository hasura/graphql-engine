import React from 'react';
import gql from 'graphql-tag';
import { Subscription } from 'react-apollo';
import {
  Alert,
} from 'react-bootstrap';
import {SUBSCRIPTION_ONLINE_USERS} from './GraphQL.jsx';

export const Users = () => (
  <Subscription subscription={gql`${SUBSCRIPTION_ONLINE_USERS}`}>
    {({ loading, error, data }) => {
       if (loading) return <span>Loading...</span>;
       if (error) return <span>Error :</span>;
       return (
         <Alert bsStyle="info">
           👥 Online users: {data.online_users[0].count}
         </Alert>
       );
    }}
  </Subscription>
)
