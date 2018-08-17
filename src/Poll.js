import React from 'react';
import {
  Query,
  ApolloConsumer,
} from 'react-apollo';
import gql from 'graphql-tag';
import {
  Button,
  ButtonGroup,
} from 'react-bootstrap';
import {
  userId,
} from './session';
import { Result } from './Result.react.js';

const QUERY_GET_POLL = gql`
  query {
    poll {
      id
      question
      options {
        id
        text
      }
    }
  }`;

const MUTATION_VOTE = gql`
mutation vote($optionId: uuid!, $userId: uuid!) {
  insert_vote(objects:[{
    option_id: $optionId,
    created_by_user_id: $userId
  }]) {
    returning {
      id
    }
  }
}
`;

const Poll = () => (
  <ApolloConsumer>
    {client => (
      <Query query={QUERY_GET_POLL}>
        {({ loading, error, data }) => {
           if (loading) return <p>Loading...</p>;
           if (error) return <p>Error :</p>;
           return (
             <div>
               {
                 data.poll.map(poll => (
                   <div key={poll.id}>
                     <div><br/>{`${poll.question}`}</div>
                     <ButtonGroup>
                     {
                       poll.options.map(option => (
                         <Button bsStyle="primary"
                                 onClick={() => {
                                     client.mutate({
                                       mutation: MUTATION_VOTE,
                                       variables: {
                                         optionId: option.id,
                                         userId,
                                       },
                                     });
                                 }}
                                 key={option.id}
                           >
                           {`${option.text}`}
                         </Button>
                       ))
                     }
                     </ButtonGroup>
                     <Result pollId={poll.id} />
                   </div>
                 ))
               }
             </div>
           );
        }}
      </Query>
    )}
  </ApolloConsumer>
);

export default Poll;
