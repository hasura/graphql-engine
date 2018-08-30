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
  mutation ($userId: Int!){
    insert_user_typing(objects: [
      {
        user_id: $userId,
        last_typed: "now()"
      }
    ],
      on_conflict: {
        constraint: user_typing_pkey,
        action: update
      }
    ) {
      returning {
        user_id
      }
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
            if (loading) {
              return "";
            }
            const sendMessage = () => {
              insert_message();
              this.setState({
                text: ""
              });
            }
            return (
              <form onSubmit={sendMessage}>
                <div className="textboxWrapper">
                  <TypingIndicator userId={this.props.userId} />
                  <input
                    id="textbox"
                    className="textbox typoTextbox"
                    value={this.state.text}
                    autoFocus={true}
                    onFocus={() => this.emitTypingEvent(client.mutate)}
                    onChange={(e) => {
                      this.setState({ text: e.target.value })
                    }}
                    autoComplete="off"
                  />
                  <button
                    className="sendButton typoButton"
                    onClick={sendMessage}
                  > Send </button>
                </div>
              </form>
            )
          }
        }

      </Mutation>
    )
  }
}
