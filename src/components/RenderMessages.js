import React from 'react';
import { Query, ApolloConsumer } from 'react-apollo';
import gql from 'graphql-tag';
import '../App.js';
import Banner from './Banner';
import MessageTrigger from './MessageTrigger';

const fetchMessages = gql`
  query ($last_received_id: Int, $last_received_ts: timestamptz){
    message (
      order_by: timestamp_asc
      where: {
        _and: {
          id: {
            _neq: $last_received_id
          },
          timestamp: {
            _gte: $last_received_ts
          }
        }
        
      }
    ) {
      id
      text
      username
      timestamp
    }
  }
`;

const fetchQuery = `
  query ($last_received_id: Int, $last_received_ts: timestamptz){
    message (
      order_by: timestamp_asc
      where: {
        _and: {
          id: {
            _neq: $last_received_id
          },
          timestamp: {
            _gte: $last_received_ts
          }
        }
        
      }
    ) {
      id
      text
      username
      timestamp
    }
  }
`;

export default class RenderMessages extends React.Component {
  constructor() {
    super();
    this.state = {
      messages: [],
      newMessages: [],
      loading: true,
      error: null,
    }
  } 


  

  async componentWillMount() {
    if (!this.props.refetch) {
      this.props.setRefetch(this.refetch);
    } 
    const messages = await this.fetchChats();
    this.setState({
      ...this.state,
      messages: messages,
      loading: false,
      bottom: false
    });
    this.scrollToBottom();
  }

  componentWillUnmount() {
    window.removeEventListener("scroll", this.handleScroll);
  }

  getLastReceivedVars = () => {
    const { messages, newMessages } = this.state;
    if (newMessages.length === 0) {
      if (messages.length !== 0) {
        return {
          last_received_id: messages[messages.length - 1].id,
          last_received_ts: messages[messages.length - 1].timestamp
        }
      } else {
        return {
          last_received_id: -1,
          last_received_ts: "2018-08-21T19:58:46.987552+00:00"
        }
      }
    } else {
      return {
        last_received_id: newMessages[newMessages.length - 1].id,
        last_received_ts: newMessages[newMessages.length - 1].timestamp
      }
    }
  }

  fetchChats = async () => {
    const {client} = this.props;
    const variables = this.getLastReceivedVars();
    const response = await fetch(
      "https://sureeee.herokuapp.com/v1alpha1/graphql",
      {
        method: 'POST',
        body: JSON.stringify({
          query: fetchQuery,
          variables
        }),
      }
    );
    const resp = await response.json();
    if(resp.error) {
      this.setState({
        ...this.state,
        error: resp.error,
        loading: false
      });
      return [];
    }
    return resp.data.message;
  }

  addNewMessages = (messages) => {
    const newMessages = [...this.state.newMessages];
    messages.forEach((m) => {
      newMessages.push(m);
    });
    this.setState({
      ...this.state,
      newMessages
    })
  }

  addOldMessages = (messages) => {
    const oldMessages = [ ...this.state.messages ];
    messages.forEach((m) => {
      oldMessages.push(m);
    });
    this.setState({
      ...this.state,
      messages: oldMessages,
      newMessages: []
    })
  }

  refetch = async() => {
    if (!this.state.loading) {
      const newMessages = await this.fetchChats('isrefetch');
      if (this.state.bottom) {
        this.addOldMessages(newMessages);
      } else {
        this.addNewMessages(newMessages);
      }
    }
  }

  scrollToBottom = () => {
    console.log('hery');
    this.lastMessage.scrollIntoView({ behavior: "instant" });
  }

  scrollToNewMessage = () => {
    this.newMessages.scrollIntoView({ behavior: "instant" });
  }

  handleScroll = () => {
    const windowHeight = "innerHeight" in window ? window.innerHeight : document.documentElement.offsetHeight;
    const body = document.body;
    const html = document.documentElement;
    const docHeight = Math.max(body.scrollHeight, body.offsetHeight, html.clientHeight,  html.scrollHeight, html.offsetHeight);
    const windowBottom = windowHeight + window.pageYOffset;
    if (windowBottom >= docHeight) {
      this.setState({
        ...this.state,
        bottom: true
      })
    } else {
      if (this.state.bottom) {
        this.setState({
          ...this.state,
          bottom: false
        });
      }
    }
  } 

  render() {
    const { messages, newMessages, bottom, loading, error } = this.state;
    console.log(this.state);
    if (loading) {
      return "Loading";
    }
    if (error) {
      return "Error: " + error;
    }
    return (
      <div>
        {
          (!bottom && newMessages.length > 0) ?
          <Banner
             scrollToNewMessage={this.scrollToNewMessage}
             numOfNewMessages={newMessages.length}
           /> : null
        }
        <MessageList
          messages={messages}
        />
        <div
          ref={(node) => { this.newMessages = node; }}
        >
          {
            newMessages.length !== 0 ? 
            "================New Messages================" :
            null
          }
        </div>
        <MessageList
          messages={newMessages}
        />
        <div
          style={{ "height": 0 }}
          ref={(node) => { this.lastMessage = node; }}
        >
        </div>
      </div>
    );
  }
}

class MessageList extends React.Component {
  render() {
    return (
      <div>
        {
          this.props.messages.map((m, i) => {
            return (
              <div key={`${m.id} + ${i}`}>
                { m.text }
              </div> 
            );
          })
        }
      </div>
    );
  }
};

