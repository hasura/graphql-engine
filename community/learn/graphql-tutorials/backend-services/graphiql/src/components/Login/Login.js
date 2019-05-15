import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import './App.css';
import { updateGraphQLEndpoint } from '../ApiExplorer/Actions';

class LoginComponent extends React.Component {
  constructor() {
    super();
    this.state = { graphqlEndpoint: '' };
  }
  setGraphQLEndpoint(e) {
    this.setState({ ...this.state, graphqlEndpoint: e.target.value });
  }
  render() {
    const { dispatch } = this.props;
    return (
      <div>
        <div className="loginWrapper">
          <Helmet
            title="GraphiQL Online with Headers | Built by Hasura"
            description="An online version of GraphiQL. Manage headers easily. Test your GraphQL servers."
          />
          <h2 className="loginHeading"> Online GraphiQL </h2>
          <div className="login">
            <div>
              <form>
                <input
                  type="text"
                  id="username"
                  className="loginTextbox"
                  placeholder="Enter GraphQL Endpoint URL"
                  onChange={this.setGraphQLEndpoint.bind(this)}
                />
                <button
                  className="loginButton"
                  type="submit"
                  onClick={(e) => {
                    e.preventDefault();
                    const emailRegex = /^(http[s]?:\/\/){0,1}(www\.){0,1}[a-zA-Z0-9\.\-]+\.[a-zA-Z]{2,5}[\.]{0,1}/;
                    if (!emailRegex.test(this.state.graphqlEndpoint)) {
                      alert('Please enter a valid URL');
                    } else {
                      dispatch(updateGraphQLEndpoint(this.state.graphqlEndpoint));
                    }
                  }}
                >
                  <i className={'fa fa-sign-in'} />
                </button>
              </form>
            </div>
          </div>
        </div>
        <div className="footerWrapper">
          <div className="built">
            Built with <i className="fa fa-heart" /> by <a href={'http://hasura.io/'} target={'_blank'}>Hasura</a>
          </div>
          <div className="apiHasura">
            <a href="https://github.com/hasura/graphql-engine/tree/master/community/tools/graphiql-online" target={'_blank'}>
              <i className="fa fa-github" />
            </a>
          </div>
        </div>
      </div>
    );
  }
}

LoginComponent.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default LoginComponent;
