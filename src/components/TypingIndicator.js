import React from 'react';
import { Subscription } from 'react-apollo';
import moment from 'moment';
import gql from 'graphql-tag';
import '../App.css';

const getUserTyping = gql`
  subscription ($timestamp: timestamptz!, $selfId: Int ) {
    user_typing (
      where: {
        _and: {
          last_typed: {
            _gt: $timestamp
          },
          user_id: {
            _neq: $selfId
          }
        }
      },
      order_by: last_typed_desc,
      limit: 1
    ){
      user_id
      last_typed
      user {
        username
      }
    }
  }
`;


class TypingIndicator extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      time: moment().subtract(2, 'seconds').format(),
    }
  }
  componentDidMount() {
    setInterval(
      () => this.setState({ time: moment().subtract(2, 'seconds').format()}),
      5000
    );
  }
  render() {
    return (
      <div className="typingIndicator">
        <Subscription
          subscription={getUserTyping}
          variables={{
            timestamp: this.state.time,
            selfId: this.props.userId
          }}
        >
          {
            ({ data, loading, error}) => {
              if (loading) { return ""; }
              if (error) { return ""; }
              if (data.user_typing.length === 0 || data.user_id === this.props.userId ) {
                return "";
              } else {
                return `${data.user_typing[0].user.username} is typing ...`;
              }
            }
          }
        </Subscription>
      </div>
    )
  }
  
};



















export default TypingIndicator;

