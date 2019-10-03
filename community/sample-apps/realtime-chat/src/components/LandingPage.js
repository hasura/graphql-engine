import React from "react";
import PropTypes from "prop-types";
import gql from 'graphql-tag';
import { Mutation } from 'react-apollo';
import "../App.css";

const addUser = gql`
  mutation ($username: String!) {
    insert_user (
      objects: [{
        username: $username
      }]
    ) {
      returning {
        id
        username
      }
    }
  }
`;

const LandingPage = (props) => {
  const reactLogo = require("../images/React-logo.png");
  const graphql = require("../images/graphql.png");
  const hasuraLogo = require("../images/green-logo-white.svg");
  const apolloLogo = require("../images/apollo.png");
  const rightImg = require("../images/chat-app.png");
  const handleKeyPress = (key, mutate, loading) => {
    if (!loading && key.charCode === 13) {
      mutate();
    }
  };
  return (
    <Mutation
      mutation={addUser}
      variables={{
        username: props.username
      }}
      onCompleted={(data) => {
        props.login(data.insert_user.returning[0].id);
      }}
      onError={() => {
        alert('Please try again with a different username.')
        props.setUsername('');
      }}
    >
      {
        (insert_user, { data, loading, error}) => {
          return (
            <div className="container-fluid minHeight">
              <div className="bgImage">
              </div>
              <div>
                <div className="headerWrapper">
                  <div className="headerDescription">
                    Realtime Chat App
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
                            <div className="col-md-8 col-sm-8 col-xs-8 noPadd">
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
                            Explore the Hasura GraphQL backend and try out some queries & mutations
                          </div>
                        </div>
                      </div>
                      <div className="appStack removePaddBottom">
                        <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                          <i className="fas fa-check-square checkBox"></i>
                        </div>
                        <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                          <div className="description removePaddBottom">
                            What you get...
                          </div>
                          <div className="addPaddTop">
                            <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                              <i className="em em-hammer_and_wrench"></i>
                            </div>
                            <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                              <div className="description removePaddBottom">
                                Source code
                              </div>
                            </div>
                          </div>
                          <div className="addPaddTop">
                            <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                              <i className="em em-closed_lock_with_key"></i>
                            </div>
                            <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                              <div className="description removePaddBottom">
                                Access to GraphQL Backend
                              </div>
                            </div>
                          </div>
                          <div className="addPaddTop">
                            <div className="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                              <i className="em em-zap" />
                            </div>
                            <div className="col-md-11 col-sm-11 col-xs-10 noPadd">
                              <div className="description removePaddBottom">
                                Full Tutorial
                              </div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                    <div className="formGroupWrapper">
                      <div className="input-group inputGroup">
                        <input
                          type="text"
                          className="form-control"
                          placeholder="Enter your username"
                          value={props.username}
                          onChange = {(e) => props.setUsername(e.target.value)}
                          onKeyPress={(key) => handleKeyPress(key, insert_user, loading)}
                          disabled={loading}
                        />
                        <div className="input-group-append groupAppend">
                          <button
                            className="btn btn-outline-secondary"
                            type="submit"
                            onClick={(e) => {
                              e.preventDefault();
                              if (props.username.match(/^[a-z0-9_-]{3,15}$/g)) {
                                insert_user();
                              } else {
                                alert("Invalid username. Spaces and special characters not allowed. Please try again");
                                props.setUsername('');
                              }
                            }}
                            disabled={loading || props.username === ''}
                          >
                            { loading ? 'Please wait ...' : 'Get Started'}
                          </button>
                        </div>
                      </div>
                    </div>
                    {/*
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
                    */}
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
    </Mutation>
  );
}

LandingPage.propTypes = {
  auth: PropTypes.object,
  isAuthenticated: PropTypes.bool
};

export default LandingPage;
