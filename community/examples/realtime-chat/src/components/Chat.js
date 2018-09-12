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

const emitOnlineEvent = gql`
  mutation ($userId: Int!){
    insert_user_online(objects: [
      {
        user_id: $userId,
        last_seen: "now()"
      }
    ],
      on_conflict: {
        constraint: user_online_pkey,
        action: update
      }
    ) {
      returning {
        user_id
      }
    }
  }
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


  async componentDidMount() {
    // Emit and event saying the user is online every 5 seconds
    setInterval(
      async () => {
        await this.props.client.mutate({
          mutation: emitOnlineEvent,
          variables: {
            userId: this.props.userId
          }
        });
      },
      1000 
    );
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
          userId={this.props.userId}
          username={username}
        />
      </div>
    );
  }
};

export default Chat;
