import React, { Component } from "react";
import PropTypes from "prop-types";
import { Navbar, Button } from "react-bootstrap";
import "../styles/App.css";

import TodoPublicWrapper from "./Todo/TodoPublicWrapper";
import TodoPrivateWrapper from "./Todo/TodoPrivateWrapper";
import OnlineUsers from "./OnlineUsers/OnlineUsers";


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
        className="btn-margin logoutBtn"
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

    const bodyContent = (
      <div className="container-fluid noPadd">
        <div className="col-xs-12 col-md-12 col-lg-9 col-sm-12 noPadd">
          <div className="col-md-6 col-sm-12">
            <div className="wd95 addPaddTopBottom">
              <div className="sectionHeader">Personal todos</div>
              <TodoPrivateWrapper />
            </div>
          </div>
          <div className="col-xs-12 col-md-6 col-sm-12 grayBgColor todoMainWrapper commonBorRight">
            <div className="wd95 addPaddTopBottom">
              <div className="sectionHeader">Public todos</div>
              <TodoPublicWrapper />
            </div>
          </div>
        </div>
        <div className="col-xs-12 col-lg-3 col-md-12 col-sm-12 noPadd">
          <OnlineUsers />
        </div>
      </div>
    );

    return (
      <div>
        <Navbar fluid className="removeMarBottom">
          <Navbar.Header className="navheader">
            <Navbar.Brand className="navBrand">
              GraphQL Tutorial App
            </Navbar.Brand>

            {isAuthenticated() ? logoutButton : loginButton}

          </Navbar.Header>
        </Navbar>

        {isAuthenticated() ? bodyContent : ''}

      </div>
    );
  }
}

App.propTypes = {
  history: PropTypes.object,
  auth: PropTypes.object
};

export default App;
