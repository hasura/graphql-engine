import React, { Component } from "react";
import PropTypes from "prop-types";
import "../../styles/App.css";
import { Link } from "react-router-dom";
class LandingPage extends Component {
  login() {
    this.props.auth.login();
  }
  logout() {
    this.props.auth.logout();
  }
  render() {
    const { isAuthenticated } = this.props.auth;
    const reactLogo = require("../../images/React-logo.png");
    const authLogo = require("../../images/auth.png");
    const graphql = require("../../images/graphql.png");
    const hasuraLogo = require("../../images/green-logo-white.svg");
    const apolloLogo = require("../../images/apollo.png");
    const rightImg = require("../../images/right-img.png");

    return (
      <div className="container-fluid gradientBgColor minHeight">
        <div>
          <div className="headerWrapper">
            <div className="headerDescription">
              {isAuthenticated() && (
                <Link to="/home">Realtime React Todo App Demo</Link>
              )}
              {!isAuthenticated() && <span>Realtime React Todo App Demo</span>}
            </div>
            <div className="loginBtn">
              {!isAuthenticated() && (
                <button
                  id="qsLoginBtn"
                  bsStyle="primary"
                  className="btn-margin logoutBtn"
                  onClick={this.login.bind(this)}
                >
                  Log In
                </button>
              )}
              {isAuthenticated() && (
                <button
                  id="qsLogoutBtn"
                  bsStyle="primary"
                  className="btn-margin logoutBtn"
                  onClick={this.logout.bind(this)}
                >
                  Log Out
                </button>
              )}
            </div>
          </div>
          <div className="mainWrapper">
            <div className="col-md-5 col-sm-6 col-xs-12 noPadd">
              <div className="appstackWrapper">
                <div className="appStack">
                  <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                    <i className="em em---1" />
                  </div>
                  <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div className="description">
                      Try out a realtime app that uses
                    </div>
                    <div className="appStackIconWrapper">
                      <div className="col-md-4 col-sm-4 col-xs-4 noPadd">
                        <div className="appStackIcon">
                          <img
                            className="img-responsive"
                            src={reactLogo}
                            alt="React logo"
                          />
                        </div>
                      </div>
                      <div className="col-md-4 col-sm-4 col-xs-4 noPadd">
                        <div className="appStackIcon">
                          <img
                            className="img-responsive"
                            src={authLogo}
                            alt="Auth0 logo"
                          />
                        </div>
                      </div>
                      <div className="col-md-4 col-sm-4 col-xs-4 noPadd">
                        <div className="appStackIcon">
                          <img
                            className="img-responsive"
                            src={graphql}
                            alt="GraphQL logo"
                          />
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div className="appStack">
                  <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                    <i className="em em-rocket" />
                  </div>
                  <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div className="description">Powered by</div>
                    <div className="appStackIconWrapper">
                      <div className="col-md-4 col-sm-4 col-xs-4 noPadd">
                        <div className="appStackIcon">
                          <img
                            className="img-responsive"
                            src={apolloLogo}
                            alt="apollo logo"
                          />
                        </div>
                      </div>
                      <div className="col-md-4 col-sm-4 col-xs-4 noPadd">
                        <div className="appStackIcon">
                          <img
                            className="img-responsive"
                            src={hasuraLogo}
                            alt="Hasura logo"
                          />
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div className="appStack">
                  <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                    <i className="em em-sunglasses" />
                  </div>
                  <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div className="description removePaddBottom">
                      Explore the Hasura console and try out some queries &
                      mutations
                    </div>
                  </div>
                </div>
                <div className="appStack removePaddBottom">
                  <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                    <i className="em em-zap" />
                  </div>
                  <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div className="description removePaddBottom">
                      Full tutorial coming soon!
                    </div>
                  </div>
                </div>
              </div>
              <div className="footer">
                Built with
                <i className="fas fa-heart" />
                by{" "}
                <a
                  href="https://hasura.io/"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Hasura
                </a>
              </div>
            </div>
            <div className="tutorialImg col-md-6 col-sm-6 col-xs-12 hidden-xs noPadd">
              <img className="img-responsive" src={rightImg} alt="View" />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

LandingPage.propTypes = {
  auth: PropTypes.object,
  isAuthenticated: PropTypes.bool
};

export default LandingPage;
