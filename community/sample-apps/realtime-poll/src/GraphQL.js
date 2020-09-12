import React from 'react';
import { Card } from 'react-bootstrap';

const QUERY_GET_POLL = `
query {
  poll (limit: 10) {
    id
    question
    options (order_by: {id:desc}){
      id
      text
    }
  }
}`;

const MUTATION_VOTE = `
mutation vote($optionId: uuid!, $userId: uuid!) {
  insert_vote(objects:[{
    option_id: $optionId,
    created_by_user_id: $userId
  }]) {
    returning {
      id
    }
  }
}`;

const SUBSCRIPTION_RESULT = `
subscription getResult($pollId: uuid!) {
  poll_results (
    order_by: {option_id:desc},
    where: { poll_id: {_eq: $pollId} }
  ) {
    option_id
    option { id text }
    votes
  }
}`;

const SUBSCRIPTION_ONLINE_USERS = `
subscription getOnlineUsersCount {
  online_users {
    count
  }
}`;

const MUTATION_MARK_USER_ONLINE = `
mutation userOnline($uuid: uuid) {
  update_user(
    where: {id: {_eq: $uuid}},
    _set : { online_ping: true }
  ) {
    affected_rows
    returning {
      last_seen_at
    }
  }
}`;

const MUTATION_NEW_USER = `
mutation newUser($uuid: uuid) {
  insert_user (
    objects:[{ id: $uuid }]
  ) {
    returning {
      id
      created_at
    }
  }
}`;

const GraphQL = () => (
  <div className="container">
    <div className="col-md-12 cardGraphQL">
      <Card>
        <Card.Header>GraphQL Queries/Mutations/Subscriptions in this page</Card.Header>
        <Card.Body>
          <div className="row">
            <div className="col-md-4">
              Get the Poll question and options:
            <pre>{QUERY_GET_POLL}</pre>

            Create a new user:
            <pre>{MUTATION_NEW_USER}</pre>
            </div>
            <div className="col-md-4">
              Cast a vote:
            <pre>{MUTATION_VOTE}</pre>

            Mark user online:
            <pre>{MUTATION_MARK_USER_ONLINE}</pre>
            </div>
            <div className="col-md-4">
              Show live results:
            <pre>{SUBSCRIPTION_RESULT}</pre>

            Get real-time number of users:
            <pre>{SUBSCRIPTION_ONLINE_USERS}</pre>
            </div>
          </div>
        </Card.Body>
      </Card>
    </div>
  </div>
)

export {
  GraphQL,
  QUERY_GET_POLL,
  MUTATION_VOTE,
  SUBSCRIPTION_RESULT,
  SUBSCRIPTION_ONLINE_USERS,
  MUTATION_MARK_USER_ONLINE,
  MUTATION_NEW_USER,
};
