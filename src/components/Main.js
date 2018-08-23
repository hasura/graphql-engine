import React from 'react';
import Chat from './Chat';
import Login from './Login';
import '../App.css';

export default class Main extends React.Component {

  constructor() {
    super();
    this.state = {
      isLoggedIn: true,
      username:"rishi"
    };
  } 

  setUsername = (username) => {
    this.setState({
      ...this.state,
      username
    }) 
  }

  login = () => {
    this.setState({
      ...this.state,
      isLoggedIn: true
    });
  }

  render() {
    const { username, isLoggedIn } = this.state;
    return (
      <div className="App">
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


