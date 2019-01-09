import React, { Component } from 'react';
import {
  Query,
  Mutation,
} from 'react-apollo';
import gql from 'graphql-tag';
import {
  Button,
  FormGroup,
  Radio,
} from 'react-bootstrap';
import { Result } from './Result';
import { Users } from './Users'
import {
  QUERY_GET_POLL,
  MUTATION_VOTE,
} from './GraphQL';


class PollQuestion extends Component {
  constructor (props) {
    super(props);
    this.state = {optionId: '', userId: props.userId, voteBtnText: '🗳 Vote', voteBtnStyle: 'primary'};
  }

  handleOptionChange = (e) => {
    this.setState({ ...this.state, optionId: e.currentTarget.value });
  }

  onMutationCompleted = () => {
    this.setState({ ...this.state, voteBtnText: '👍 Done', voteBtnStyle: 'success' });
    window.setTimeout(() => {
      this.setState({ ...this.state, voteBtnText: '🗳️ Vote', voteBtnStyle: 'primary' });
    }, 3000);
  }

  onMutationError = () => {
    this.setState({ ...this.state, voteBtnText: 'Error 😞 Try again', voteBtnStyle: 'danger' });
  }

  render () {
    return (
      <Mutation
        mutation={gql`${MUTATION_VOTE}`}
        onCompleted={this.onMutationCompleted}
        onError={this.onMutationError}
      >
      {(vote) => (
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
        </div>
      )}
      </Mutation>
    );
  }
};


const Poll = ({userId}) => (
  <Query query={gql`${QUERY_GET_POLL}`}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div className="container">
           {
             data.poll.map(poll => (
               <div key={poll.id} className="pollWrapper wd100">
                 <div className="displayFlex">
                   <div className="col-md-4 pollSlider">
                     <PollQuestion poll={poll} userId={userId} />
                     <div className="online-users">
                       <Users />
                     </div>
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
