import React from 'react';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';
import '../App.css';

const subscriptionQuery = gql`
  subscription {
    message ( order_by: id_desc limit: 1) {
      id
      username
      text
      timestamp
    }
  }  
`;

const MessageTrigger = (props) => {
  const { messages, isNew, addMessage, readAll } = props;
  return (
    <Subscription
      subscription={subscriptionQuery}
    >
      { ({data, error, loading}) => {
        if (data && data.message && data.message.length !== 0) {
          if (messages[messages.length - 1].timestamp < data.message[0].timestamp) {
            addMessage({
              ...data.message[0],
            }, isNew);
            if (!isNew) {
              readAll();
            }
          } 
        }
        return null;
      }}
    </Subscription>      
  );
}

export default MessageTrigger;