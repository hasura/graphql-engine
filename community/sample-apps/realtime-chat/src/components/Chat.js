import React from 'react';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';
import ChatWrapper from './ChatWrapper';
import '../App.css';

const subscribeToNewMessages = gql`
  subscription {
    message ( order_by: {id:desc} limit: 1) {
      id
      username
      text
      timestamp
    } }
`;

const emitOnlineEvent = gql`
  mutation ($userId:Int!){
    update_user (
      _set: {
        last_seen: "now()"
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
      3000
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
          subscription={subscribeToNewMessages}
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
        <footer className="App-footer">
          <div className="hasura-logo">
            <img src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_black.svg" onClick={() => window.open("https://hasura.io")} alt="Powered by Hasura"/>
            &nbsp; | &nbsp;
            <a href="/console" target="_blank" rel="noopener noreferrer">
              Backend
            </a>
            &nbsp; | &nbsp;
            <a href="https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/realtime-chat" target="_blank" rel="noopener noreferrer">
              Source
            </a>
            &nbsp; | &nbsp;
            <a href="https://hasura.io/blog/building-a-realtime-chat-app-with-graphql-subscriptions-d68cd33e73f" target="_blank" rel="noopener noreferrer">
              Blogpost
            </a>
          </div>
          <div className="footer-small-text"><span>(The database resets every 24 hours)</span></div>
        </footer>
      </div>
    );
  }
};

export default Chat;
