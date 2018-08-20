import React, { Component }from 'react';
import {
  Query,
  Mutation,
} from 'react-apollo';
import gql from 'graphql-tag';
import {
  Button,
  ButtonGroup,
  FormGroup,
  Radio,
  Well,
} from 'react-bootstrap';
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
}
`;

class PollQuestion extends Component {
  constructor (props) {
    super(props);
    this.state = {optionId: '', userId: props.userId};
  }

  handleOptionChange = (e) => {
    this.setState({ ...this.state, optionId: e.currentTarget.value })
  }

  render () {
    return (
      <Mutation mutation={gql`${MUTATION_VOTE}`}>
        {(vote, { data }) => (
          <div>
            <div>{this.props.poll.question}</div>
            <form
              onSubmit={e => {
                  e.preventDefault();
                  vote({
                    variables: {
                      optionId: this.state.optionId,
                      userId: this.state.userId
                    }
                  });
              }}
            >
              <FormGroup>
                {
                  this.props.poll.options.map(option => (
                    <Radio
                      key={option.id}
                      value={option.id}
                      name="voteCandidate"
                      onChange={this.handleOptionChange}
                    >
                      {option.text}
                    </Radio>
                  ))
                }
              </FormGroup>
              <Button type="submit">Vote</Button>
            </form>
            <div>
              <Well><pre>{MUTATION_VOTE}</pre></Well>
            </div>
          </div>
        )}
      </Mutation>
    );
  }
};


const Poll = ({userId}) => (
  <Query query={QUERY_GET_POLL}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div>
           {
             data.poll.map(poll => (
               <div key={poll.id}>
                 <PollQuestion poll={poll} userId={userId} />
                 <Result pollId={poll.id} />
               </div>
             ))
           }
         </div>
       );
    }}
  </Query>
);

export default Poll;
