import React, { Component } from "react";
import PropTypes from "prop-types";
import { Navbar, Button } from "react-bootstrap";
import "../styles/App.css";

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

  componentDidMount() {
    if (this.props.auth.isAuthenticated()) {
      this.props.history.push("/home");
    }
  }

  render() {
    const { isAuthenticated } = this.props.auth;

    return (
      <div>
        <Navbar fluid className="removeMarBottom">
          <Navbar.Header className="navheader">
            <Navbar.Brand className="navBrand">
              GraphQL Tutorial App
            </Navbar.Brand>
            {!isAuthenticated() && (
              <Button
                id="qsLoginBtn"
                bsStyle="primary"
                className="btn-margin logoutBtn"
                onClick={this.login.bind(this)}
              >
                Log In
              </Button>
            )}
            {isAuthenticated() && (
              <Button
                id="qsLogoutBtn"
                bsStyle="primary"
                className="btn-margin logoutBtn"
                onClick={this.logout.bind(this)}
              >
                Log Out
              </Button>
            )}
          </Navbar.Header>
        </Navbar>
      </div>
    );
  }
}

App.propTypes = {
  history: PropTypes.object,
  auth: PropTypes.object
};

export default App;
