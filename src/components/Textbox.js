import React from 'react';
import { Mutation } from 'react-apollo';
import gql from 'graphql-tag';
import "../App.css";

const insertMessage = gql`
  mutation insert_message ($message: message_insert_input! ){
    insert_message (
      objects: [$message]
    ) {
      returning {
        id
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

  componentDidUpdate() {
    
  }

  render() {
    return (
      <Mutation
        mutation={insertMessage}
        variables={{
          message: {
            username: this.props.username,
            text: this.state.text
          }
        }}
      >
        {
          (insert_message, { data, loading, error, called}) => {
            if (loading) {
              return "";
            }
            const sendMessage = () => {
              insert_message();
              this.setState({ text: "" })
              this.textbox.focus();
              document.getElementById("lastMessage").scrollIntoView({ behavior: 'instant'})
            }
            return (
              <form onSubmit={sendMessage}>
                <div className="textboxWrapper">
                  <input
                    ref={(node) => {this.textbox = node;}}
                    className="textbox"
                    value={this.state.text}
                    onChange={(e) => {
                      this.setState({ text: e.target.value })
                    }}
                  />
                  <button
                    className="sendButton"
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