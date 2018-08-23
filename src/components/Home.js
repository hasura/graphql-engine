import React from 'react';
import ChatComponent from './ChatComponent';
import Login from './Login';
import '../App.css';

export default class Home extends React.Component {

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
            <ChatComponent
              username={username}
            />  
          )
        }
      </div>
    )
  }
};


