import gql from "graphql-tag";
import React from "react";
import { Card } from "react-bootstrap";

const QUERY_GET_POLL = gql`
  query {
    poll(limit: 10) {
      id
      question
      options(order_by: { id: desc }) {
        id
        text
      }
    }
  }
`;

const MUTATION_VOTE = gql`
  mutation vote($optionId: uuid!, $userId: uuid!) {
    insert_vote(
      objects: [{ option_id: $optionId, created_by_user_id: $userId }]
    ) {
      returning {
        id
      }
    }
  }
`;

const SUBSCRIPTION_RESULT = gql`
  subscription getResult($pollId: uuid!) {
    poll_results(
      order_by: { option_id: desc }
      where: { poll_id: { _eq: $pollId } }
    ) {
      option_id
      option {
        id
        text
      }
      votes
    }
  }
`;

const SUBSCRIPTION_ONLINE_USERS = gql`
  subscription getOnlineUsersCount {
    online_users {
      count
    }
  }
`;

const MUTATION_MARK_USER_ONLINE = gql`
  mutation userOnline($uuid: uuid) {
    update_user(where: { id: { _eq: $uuid } }, _set: { online_ping: true }) {
      affected_rows
      returning {
        last_seen_at
      }
    }
  }
`;

const MUTATION_NEW_USER = gql`
  mutation newUser($uuid: uuid) {
    insert_user(objects: [{ id: $uuid }]) {
      returning {
        id
        created_at
      }
    }
  }
`;

const GraphQLQueryList = () => (
  <div className="container">
    <div className="col-md-12 cardGraphQL">
      <Card>
        <Card.Header>
          GraphQL Queries/Mutations/Subscriptions in this page
        </Card.Header>
        <Card.Body>
          <div className="row">
            <div className="col-md-4">
              Get the Poll question and options:
              <pre>{QUERY_GET_POLL.loc.source.body}</pre>
              Create a new user:
              <pre>{MUTATION_NEW_USER.loc.source.body}</pre>
            </div>
            <div className="col-md-4">
              Cast a vote:
              <pre>{MUTATION_VOTE.loc.source.body}</pre>
              Mark user online:
              <pre>{MUTATION_MARK_USER_ONLINE.loc.source.body}</pre>
            </div>
            <div className="col-md-4">
              Show live results:
              <pre>{SUBSCRIPTION_RESULT.loc.source.body}</pre>
              Get real-time number of users:
              <pre>{SUBSCRIPTION_ONLINE_USERS.loc.source.body}</pre>
            </div>
          </div>
        </Card.Body>
      </Card>
    </div>
  </div>
);

export {
  GraphQLQueryList,
  QUERY_GET_POLL,
  MUTATION_VOTE,
  SUBSCRIPTION_RESULT,
  SUBSCRIPTION_ONLINE_USERS,
  MUTATION_MARK_USER_ONLINE,
  MUTATION_NEW_USER,
};
