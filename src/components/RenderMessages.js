import React from 'react';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';
import '../App.js';
import Banner from './Banner';
import MessageTrigger from './MessageTrigger';

export default class RenderMessages extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      messages: props.messages,
      bottom: true
    }
  }

  componentDidMount() {
    window.addEventListener("scroll", this.handleScroll);
    this.scrollToLastMessage();
    this.handleScroll();
    this.markAllMessagesAsRead();
  }

  componentWillUnmount() {
    window.removeEventListener("scroll", this.handleScroll);
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

  addNewMessageToState = (payload, isNew) => {
    const messages = [ ...this.state.messages];
    messages.push({ ...payload, isNew});
    this.setState({
      ...this.state,
      messages,
      allRead: !isNew,
    });
    if (!isNew) {
      this.scrollToLastMessage();
    }
  }

  markAllMessagesAsRead = () => {
    if (!this.state.allRead) {
      const readMessages = this.state.messages.map((m) => {
        if (m.isNew) {
          return {
            ...m,
            isNew: false
          }
        }
        return m;
      });
      this.setState({
        ...this.state,
        messages: readMessages,
        allRead: true,
        bottom: true
      });
    }
  }

  scrollToNewMessage = () => {
    this.newMessage.scrollIntoView({ behavior: "instant" });
  }

  scrollToLastMessage = () => {
    this.lastMessage.scrollIntoView({ behavior: "instant" }); 
  }
 
  render() {
    const { bottom, messages, allRead } = this.state;
    const newMessageBanner = (
      <div
        ref={(node) => { this.newMessage = node; }}
      >
        ======================New messages========================
      </div>
    );
    return (
      <div>
        {
          (!bottom && !allRead) ?
          <Banner
            className="banner"
            scrollToNewMessage={this.scrollToNewMessage}
            numOfNewMessages={messages.filter((m) => m.isNew).length}
          /> :
          null
        }
        <div> 
          <RenderOldMessages messages={this.state.messages} />
           <MessageTrigger
            isNew={!bottom}
            addMessage={this.addNewMessageToState}
            messages={this.state.messages}
            readAll={this.markAllMessagesAsRead}
          />
          {
            (!allRead) ? newMessageBanner : null
          }
          <RenderNewMessages
            messages={this.state.messages}
          />
          <div
            style={{ height: 0 }}
            ref={(node) => {this.lastMessage = node}}
          ></div>
        </div>
      </div> 
    )
  }
}



const RenderOldMessages = (props) => {
  return props.messages.filter((message) => !message.isNew).map((m, key) =>{
    return (
      <div key={key} className="message">
        {m.username}: {m.text}
      </div>
    )
  })
};

class RenderNewMessages extends React.Component {
  render () {
    return this.props.messages.filter((message) => message.isNew).map((m, key) =>{
      return (
        <div key={key} className="newMessage">
          {m.username}: {m.text}
        </div>
      );
    });
  }
};