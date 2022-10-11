import { useState } from 'react';
import { gql, useMutation } from '@apollo/client';
import TypingIndicator from './TypingIndicator';
import '../App.css';

const insertMessage = gql`
  mutation insert_message($message: message_insert_input!) {
    insert_message_one(object: $message) {
      id
      timestamp
      text
      username
    }
  }
`;

const emitTypingEventGql = gql`
  mutation ($userId: Int!) {
    update_user_by_pk(
      pk_columns: { id: $userId }
      _set: { last_typed: "now()" }
    ) {
      id
    }
  }
`;

export default function Textbox(props) {
  const [text, setText] = useState('');

  const [insertMessageHandler, { client }] = useMutation(insertMessage, {
    variables: {
      message: {
        username: props.username,
        text: text,
      },
      update: (cache, { data: { insert_message_one } }) => {
        props.mutationCallback({
          id: insert_message_one.id,
          timestamp: insert_message_one.timestamp,
          username: insert_message_one.username,
          text: insert_message_one.text,
        });
      },
    },
  });

  const handleTyping = (text, mutate) => {
    const textLength = text.length;
    if ((textLength !== 0 && textLength % 5 === 0) || textLength === 1) {
      emitTypingEvent(mutate);
    }
    setText(text);
  };

  const form = (sendMessage, client) => {
    return (
      <form onSubmit={sendMessage}>
        <div className="textboxWrapper">
          <TypingIndicator userId={props.userId} />
          <input
            id="textbox"
            className="textbox typoTextbox"
            value={text}
            autoFocus={true}
            onChange={(e) => {
              handleTyping(e.target.value, client.mutate);
            }}
            autoComplete="off"
          />
          <button className="sendButton typoButton" onClick={sendMessage}>
            {' '}
            Send{' '}
          </button>
        </div>
      </form>
    );
  };

  const emitTypingEvent = async (mutate) => {
    if (props.userId) {
      await mutate({
        mutation: emitTypingEventGql,
        variables: {
          userId: props.userId,
        },
      });
    }
  };
  const sendMessage = (e) => {
    e.preventDefault();
    if (text === '') {
      return;
    }
    insertMessageHandler();
    setText('');
  };
  return form(sendMessage, client);
}
