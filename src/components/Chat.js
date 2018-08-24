import React from 'react';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';
import ChatWrapper from './ChatWrapper';

const subscribeToEvent = gql`
  subscription {
    message ( order_by: id_desc limit: 1) {
      id
      username
      text
      timestamp
    } }
`;

class Chat extends React.Component {

  constructor (props) {
    super(props);
    this.state = {
      username: props.username,
      refetch: null
    };
  }

  // set refetch function (coming from child <Query> component) using callback
  setRefetch = (refetch) => {
    this.setState({
      ...this.state,
      refetch
    })
  }


  /*
    Subscription is used only for event notification
    No data is bound to the subscription component
    As soon as an event occurs, the refetch() of the child component is called
  */
  render () {
    const { refetch, username } = this.state;
    return (
      <div>
        <Subscription
          subscription={subscribeToEvent}
        >
          {
            ({data, error, loading}) => {
              if (error || (data && data.message === null)) {
                console.error(error || `Unexpected response: ${data}`);
                return "Error";
              }
              if (refetch) {
                refetch();
              }
              return null;
            }
          }
        </Subscription>
        <ChatWrapper
          refetch={refetch}
          setRefetch={this.setRefetch}
          username={username}
        />
      </div>
    );
  }
};

export default Chat;
