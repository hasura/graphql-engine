import React from 'react';
import Chat from './Chat';
import Login from './Login';
import '../App.css';

export default class Main extends React.Component {

  constructor() {
    super();
    this.state = {
      isLoggedIn: false,
      username:""
    };
  } 

  // set username
  setUsername = (username) => {
    this.setState({
      ...this.state,
      username
    }) 
  }

  // check usernme and  perform login
  login = () => {
    const { username } = this.state;
    if (username.match(/^[a-z0-9_-]{3,15}$/g)) {
      this.setState({
        ...this.state,
        isLoggedIn: true
      });
    } else {
      alert("Invalid username. Spaces and special characters not allowed. Please try again");
    }
  }

  render() {
    const { username, isLoggedIn } = this.state;
    // Login if not logged in and head to chat
    return (
      <div className="app">
        {
          !isLoggedIn ? (
            <Login
              username={username}
              setUsername={this.setUsername}
              login={this.login}
            />
          ) : (
            <Chat
              username={username}
            />  
          )
        }
      </div>
    )
  }
};


