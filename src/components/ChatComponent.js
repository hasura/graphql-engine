import React from 'react';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';
import RenderMessagesProxy from './RenderMessagesProxy';
import Textbox from './Textbox';

const subscribeToEvent = gql`
  subscription {
    message ( order_by: id_desc limit: 1) {
      id
      username
      text
      timestamp
    } }
`;

class ChatComponent extends React.Component {

  constructor (props) {
    super(props);
    this.state = {
      username: props.username,
      refetch: null
    };
  }

  setRefetch = (refetch) => {
    this.setState({
      ...this.state,
      refetch
    })
  }

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
        <RenderMessagesProxy
          refetch={refetch}
          setRefetch={this.setRefetch}
          username={username}
        />
      </div>
    );
  }
};

export default ChatComponent;