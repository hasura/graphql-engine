import React, { Component } from 'react';
import { Query, Mutation } from 'react-apollo';
import gql from 'graphql-tag';
import { Button, Form } from 'react-bootstrap';
import { Result } from './Result';
import { QUERY_GET_POLL, MUTATION_VOTE } from './GraphQL';

class PollQuestion extends Component {
  constructor(props) {
    super(props);
    this.state = {
      optionId: '',
      pollId: props.poll.id,
      voteBtnText: '🗳 Vote',
      voteBtnStyle: 'primary'
    };
  }

  handleOptionChange = (e) => {
    this.setState({
      optionId: e.currentTarget.value
    });
  }

  onMutationCompleted = () => {
    this.setState({
      voteBtnText: '👍 Done',
      voteBtnStyle: 'success'
    });
    // re-authorize to vote after 5 seconds
    window.setTimeout(() => {
      this.setState({
        voteBtnText: '🗳️ Vote',
        voteBtnStyle: 'primary'
      });
    }, 5000);
  }

  onMutationError = () => {
    this.setState({
      voteBtnText: 'Error 😞 Try again',
      voteBtnStyle: 'danger'
    });
  }

  handlesubmitVote = (e, vote) => {
    e.preventDefault();
    if (!this.state.optionId) {
      this.setState({
        voteBtnText: '✋ Select an option and try again',
        voteBtnStyle: 'warning'
      });
      return
    }
    this.setState({
      voteBtnText: '🗳️ Submitting',
      voteBtnStyle: 'info'
    });
    vote({
      variables: {
        optionId: this.state.optionId,
        userId: this.props.userId
      }
    });
  }

  render() {
    return (
      <Mutation
        mutation={gql`${MUTATION_VOTE}`}
        onCompleted={this.onMutationCompleted}
        onError={this.onMutationError}
      >
        {(vote) => (
          <div className="textLeft">
            <h3>{this.props.poll.question}</h3>
            <Form className="pollForm textLeft" onSubmit={e => { this.handlesubmitVote(e, vote) }}>
              {
                this.props.poll.options.map(option => (

                  <Form.Check
                    custom
                    type="radio"
                    name="voteCandidate"
                    id={option.id}
                    key={option.id}
                    value={option.id}
                    label={option.text}
                    onChange={this.handleOptionChange}
                  />

                ))
              }
              <Button className="voteBtn info" variant={this.state.voteBtnStyle} type="submit">
                {this.state.voteBtnText}
              </Button>
            </Form>
          </div>
        )}
      </Mutation>
    );
  }
};


const Poll = ({ userId }) => (
  <div>
    <Query query={gql`${QUERY_GET_POLL}`}>
      {({ loading, error, data }) => {
        if (loading) return <p>Loading...</p>;
        if (error) {
          return <div class="alert alert-danger" role="alert"><b>Error:</b> ${error.message}</div>;
        }
        return (
          <div className="container">
            {
              data.poll.map(poll => (
                <div key={poll.id} className="pollWrapper wd100">
                  <div className="displayFlex">
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
  </div>
);

export default Poll;
