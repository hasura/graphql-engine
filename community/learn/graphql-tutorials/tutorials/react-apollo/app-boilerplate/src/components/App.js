import React, { Component } from "react";
import PropTypes from "prop-types";
import { Navbar, Button } from "react-bootstrap";
import "../styles/App.css";

import TodoPublicWrapper from "./Todo/TodoPublicWrapper";
import TodoPrivateWrapper from "./Todo/TodoPrivateWrapper";
import OnlineUsersWrapper from "./OnlineUsers/OnlineUsersWrapper";


class App extends Component {
  goTo(route) {
    this.props.history.replace(`/${route}`);
  }

  login() {
    this.props.auth.login();
  }

  logout() {
    this.props.auth.logout();
  }

  render() {
    const { isAuthenticated } = this.props.auth;

    const loginButton = (
      <Button
        id="qsLoginBtn"
        bsStyle="primary"
        className="btn-margin loginBtn"
        onClick={this.login.bind(this)}
      >
        Log In
      </Button>
    );

    const logoutButton = (
      <Button
        id="qsLogoutBtn"
        bsStyle="primary"
        className="btn-margin logoutBtn"
        onClick={this.logout.bind(this)}
      >
        Log Out
      </Button>
    );

    const loginOverlay = (
      <div className="overlay">
        { loginButton }
      </div>
    );

    return (
      <div>
        { isAuthenticated() || loginOverlay }

        <Navbar fluid className="m-bottom-0">
          <Navbar.Header className="navHeader">
            <Navbar.Brand className="navBrand">
              GraphQL Tutorial App
            </Navbar.Brand>

            {isAuthenticated() ? logoutButton : loginButton}

          </Navbar.Header>
        </Navbar>

        <div className="container-fluid p-left-right-0">
          <div className="col-xs-12 col-md-9 p-left-right-0">
            <div className="col-xs-12 col-md-6 p-top-bottom-30">
              <TodoPrivateWrapper />
            </div>
            <div className="col-xs-12 col-md-6 sliderMenu bg-gray p-top-bottom-30 border-right">
              <TodoPublicWrapper />
            </div>
          </div>
          <div className="col-xs-12 col-md-3 p-left-right-0">
            <div className="col-xs-12 col-md-12 sliderMenu bg-gray p-top-bottom-30">
              <OnlineUsersWrapper />
            </div>
          </div>
        </div>

      </div>
    );
  }
}

App.propTypes = {
  history: PropTypes.object,
  auth: PropTypes.object
};

export default App;
