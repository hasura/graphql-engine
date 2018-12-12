import React, { Component } from 'react';
import './App.css';

class App extends Component {
  goTo(route) {
    this.props.history.replace(`/${route}`)
  }

  login() {
    this.props.auth.login();
  }

  logout() {
    this.props.auth.logout();
  }

  render() {
    const { isAuthenticated } = this.props.auth;
    return (
      <div className="App container-fluid">
        <nav className="navbar navbar-default">
          <div className="container-fluid">
            <div className="navbar-header">
              <button type="button" className="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                <span className="sr-only">Toggle navigation</span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
              </button>
              <a className="navbar-brand" href="#">todos</a>
            </div>

            <div className="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
              <ul className="nav navbar-nav navbar-right">
                {
                  isAuthenticated() && (
                      <li style={{ marginTop: '3%' }}>
                        ( Logged in Successfully !! )
                      </li>
                    )
                }
                {
                  isAuthenticated() && (
                      <li>
                        <button className="btn btn-default" onClick={this.logout.bind(this)}>Logout</button>
                      </li>
                    )
                }
                {
                  !isAuthenticated() && (
                      <button className="btn btn-default" onClick={this.login.bind(this)} style={{ marginTop: '5%' }}>Login</button>
                    )
                }
              </ul>
            </div>
          </div>
        </nav>
      </div>
    );
  }
}

export default App;
