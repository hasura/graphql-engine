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
  Alert,
} from 'react-bootstrap';
import { Result } from './Result.react.js';
import { Users } from './Users.react'
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
    this.state = {optionId: '', userId: props.userId, voteBtnText: 'Vote', voteBtnStyle: 'primary'};
  }

  handleOptionChange = (e) => {
    this.setState({ ...this.state, optionId: e.currentTarget.value });
  }

  onMutationCompleted = (e) => {
    this.setState({...this.state, voteBtnText: '👍 Done', voteBtnStyle: 'success'});
    window.setTimeout(() => {
      this.setState({...this.state, voteBtnText: '🗳️ Vote', voteBtnStyle: 'primary'});
    }, 5000)
  }

  onMutationError = (e) => {
    this.setState({...this.state, voteBtnText: 'Error 😞 Try again', voteBtnStyle: 'danger'});
  }

  render () {
    return (
      <Mutation mutation={gql`${MUTATION_VOTE}`} onCompleted={this.onMutationCompleted} onError={this.onMutationError}>
        {(vote, { loading, error }) => (
          <div className="textLeft">
            <h3>{this.props.poll.question}</h3>
            <form className="pollForm textLeft"
                  onSubmit={e => {
                      e.preventDefault();
                      if (!this.state.optionId) {
                        this.setState({...this.state, voteBtnText: '✋ Select an option and try again', voteBtnStyle: 'warning'});
                        return
                      }
                      this.setState({...this.state, voteBtnText: '🗳️ Submitting', voteBtnStyle: 'info'});
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
              <Button className="voteBtn" bsStyle={this.state.voteBtnStyle} type="submit">
                {this.state.voteBtnText}
              </Button>
            </form>
            <div>
              <pre>{MUTATION_VOTE}</pre>
            </div>
            <Users />
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
               <div className="container">
                 {
                   data.poll.map(poll => (
                     <div className="pollWrapper wd100">
                       <div key={poll.id}>
                         <div className="col-md-4 pollSlider">
                           <PollQuestion poll={poll} userId={userId} />
                         </div>
                         <div className="col-md-8 pollresult">
                           <Result pollId={poll.id} />
                         </div>
                       </div>
                     </div>
                   ))
                 }
               </div>
             );
          }}
        </Query>
      );

export default Poll;
