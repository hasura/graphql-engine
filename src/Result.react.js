import React from 'react';
import {
  Subscription,
} from 'react-apollo';
import gql from 'graphql-tag';

const SUBSCRIBE_RESULT = gql`
subscription getResult($pollId: uuid!) {
  poll_results (
    where: {
      poll_id: {_eq: $pollId}
    }
  ) {
    option {
      id
      text
    }
    votes
  }
}
`;

export const Result = (pollId) => (
  <Subscription subscription={SUBSCRIBE_RESULT} variables={pollId}>
    {({ loading, error, data }) => {
      if (loading) return <p>Loading...</p>;
      if (error) return <p>Error :</p>;
      return (
        <div>
          {
            data.poll_results.map((result, i) => (
              <div key={i}>{`${result.option.text}: ${result.votes}`}</div>
            ))
          }
        </div>
      );
    }}
  </Subscription>
)
