import React from 'react';
import { Mutation } from 'react-apollo';
import gql from 'graphql-tag';
import '../App.css';

const insertMessage = gql`
  mutation insert_message ($message: message_insert_input! ){
    insert_message (
      objects: [$message]
    ) {
      returning {
        id
        timestamp
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
          this.props.mutationCallback([
            {
              id: insert_message.returning[0].id,
              timestamp: insert_message.returning[0].timestamp,
              username: this.props.username,
              text: this.state.text
            }
          ]);
          this.setState({ text: "" })
          document.getElementById("textbox").scrollIntoView({ behavior: 'instant'})
        }}
      >
        {
          (insert_message, { data, loading, error, called}) => {
            if (loading) {
              return "";
            }
            const sendMessage = () => {
              insert_message();
            }
            return (
              <form onSubmit={sendMessage}>
                <div className="textboxWrapper">
                  <input
                    id="textbox"
                    className="textbox loginTextbox"
                    value={this.state.text}
                    onChange={(e) => {
                      this.setState({ text: e.target.value })
                    }}
                  />
                  <button
                    className="sendButton loginButton"
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
