import React, { Component } from "react";
import PropTypes from "prop-types";
import moment from "moment";
import gql from "graphql-tag";
import "../../styles/App.css";
import TodoPublicWrapper from "../Todo/TodoPublicWrapper";
import TodoPrivateWrapper from "../Todo/TodoPrivateWrapper";
import OnlineUsers from "../OnlineUsers/OnlineUsers";
import { Navbar, Button } from "react-bootstrap";
class App extends Component {
  login() {
    this.props.auth.login();
  }
  logout() {
    this.props.auth.logout();
  }
  updateLastSeen = () => {
    const userId = localStorage.getItem("auth0:id_token:sub");
    const timestamp = moment().format();
    if (this.props.client) {
      this.props.client
        .mutate({
          mutation: gql`
            mutation($userId: String!, $timestamp: timestamptz!) {
              update_users(
                where: { auth0_id: { _eq: $userId } }
                _set: { auth0_id: $userId, last_seen: $timestamp }
              ) {
                affected_rows
              }
            }
          `,
          variables: {
            userId: userId,
            timestamp: timestamp
          }
        })
        .then(() => {
          // handle response if required
        })
        .catch(error => {
          console.error(error);
        });
    }
  };
  componentDidMount() {
    // eslint-disable-next-line
    const lastSeenMutation = setInterval(this.updateLastSeen.bind(this), 5000);
  }
  render() {
    const { isAuthenticated } = this.props.auth;
    if (!isAuthenticated()) {
      window.location.href = "/";
    }
    return (
      <div>
        <Navbar fluid className="removeMarBottom">
          <Navbar.Header className="navheader">
            <Navbar.Brand className="navBrand">
              React Apollo Todo App
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
        <div>
          <div className="col-xs-12 col-md-12 col-lg-9 col-sm-12 noPadd">
            <div>
              <div className="col-md-6 col-sm-12">
                <div className="wd95 addPaddTopBottom">
                  <div className="sectionHeader">Personal todos</div>
                  <TodoPrivateWrapper client={this.props.client} />
                </div>
              </div>
              <div className="col-xs-12 col-md-6 col-sm-12 grayBgColor todoMainWrapper commonBorRight">
                <div className="wd95 addPaddTopBottom">
                  <div className="sectionHeader">Public todos</div>
                  <TodoPublicWrapper client={this.props.client} />
                </div>
              </div>
            </div>
          </div>
          <div className="col-xs-12 col-lg-3 col-md-12 col-sm-12 noPadd">
            <OnlineUsers />
          </div>
        </div>
        <div className="footerWrapper">
          <span>
            <a
              href="https://react-apollo-todo-demo.hasura.app/console"
              target="_blank"
              rel="noopener noreferrer"
            >
              Backend
              <i className="fa fa-angle-double-right" />
            </a>
          </span>
          <span className="footerLinkPadd accessKey">
            <button>
              Access Key: hasurademoapp
            </button>
          </span>
          <span className="footerLinkPadd">
            <a
              href="https://github.com/hasura/graphql-engine/tree/master/community/tutorials/react-apollo-todo"
              target="_blank"
              rel="noopener noreferrer"
            >
              Github
              <i className="fa fa-angle-double-right" />
            </a>
          </span>
        </div>
      </div>
    );
  }
}

App.propTypes = {
  auth: PropTypes.object,
  isAuthenticated: PropTypes.bool
};

export default App;
