import React from 'react';
import { Mutation } from 'react-apollo';
import gql from 'graphql-tag';
import TypingIndicator from './TypingIndicator';
import '../App.css';

const insertMessage = gql`
  mutation insert_message ($message: message_insert_input! ){
    insert_message (
      objects: [$message]
    ) {
      returning {
        id
        timestamp
        text
        username
      }
    }
  }
`;

const emitTypingEvent = gql`
  mutation ($userId: Int) {
    update_user (
      _set: {
        last_typed: "now()"
      }
      where: {
        id: {
          _eq: $userId
        }
      }
    ) {
      affected_rows
    }
  }
`;

export default class Textbox extends React.Component {

  constructor(props) {
    super()
    this.state = {
      text: ""
    }
  }

  handleTyping = (text, mutate) => {
    const textLength = text.length;
    if ((textLength !== 0 && textLength % 5 === 0) || textLength === 1) {
      this.emitTypingEvent(mutate);
    }
    this.setState({ text });
  }

  emitTypingEvent = async (mutate) => {
    if (this.props.userId) {
      await mutate({
        mutation: emitTypingEvent,
        variables: {
          userId: this.props.userId
        }
      });
    }
  }

  render() {
    // Mutation component. Add message to the state of <RenderMessages> after mutation.
    return (
      <Mutation
        mutation={insertMessage}
        variables={{
          message: {
            username: this.props.username,
            text: this.state.text
          }
        }}
        update={(cache, { data: { insert_message }}) => {
          this.props.mutationCallback(
            {
              id: insert_message.returning[0].id,
              timestamp: insert_message.returning[0].timestamp,
              username: insert_message.returning[0].username,
              text: insert_message.returning[0].text,
            }
          );
        }}
      >
        {
          (insert_message, { data, loading, error, client}) => {
            const sendMessage = (e) => { 
              e.preventDefault();
              if (this.state.text === '') {
                return;
              }
              insert_message();
              this.setState({
                text: ""
              });
            }
            return this.form(sendMessage, client);
          }
        }

      </Mutation>
    )
  }

  form = (sendMessage, client) => {
    return (
      <form onSubmit={sendMessage}>
        <div className="textboxWrapper">
          <TypingIndicator userId={this.props.userId} />
          <input
            id="textbox"
            className="textbox typoTextbox"
            value={this.state.text}
            autoFocus={true}
            onChange={(e) => {
              this.handleTyping(e.target.value, client.mutate);
            }}
            autoComplete="off"
          />
          <button
            className="sendButton typoButton"
            onClick={sendMessage}
          > Send </button>
        </div>
      </form>
    );
  }
}
