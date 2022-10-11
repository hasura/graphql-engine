import React from 'react';

import PropTypes from 'prop-types';
import { Button } from '@hasura/console-oss';
import { Link } from 'react-router';
import { FaExclamationCircle } from 'react-icons/fa';

import { idTokenReceived } from '../Main/Actions';

import {
  validateOauthResponseState,
  defaultErrorMessage,
  hasOAuthLoggedIn,
} from './utils';

import { retrieveIdToken } from './Actions';
import LoadingScreen from './LoadingScreen';

class OAuthCallback extends React.Component {
  constructor() {
    super();
    this.state = {
      isErrorAuthenticating: false,
      errorMsg: {
        ...defaultErrorMessage,
      },
    };
  }
  componentDidMount() {
    const { location } = this.props;
    const { query } = location;
    const { code, state } = query;
    if (code && validateOauthResponseState(state)) {
      this.props
        .dispatch(retrieveIdToken(code))
        .then(data => {
          /* Once the refresh/token is received, keep a state
           * in LS to capture the fact that user has already
           * performed oauth once and can continue to do silent
           * auth until opted out
           * */
          hasOAuthLoggedIn(true);
          this.props.dispatch(idTokenReceived(data));
        })
        .catch(err => {
          this.verificationError(err);
        });
    } else {
      const { error, error_description } = query;
      const err = {};
      if (error || error_description) {
        err.error = error;
        err.error_description = error_description;
        /*
        const err = {
          error,
          error_description,
        };
        */
      } else {
        /* Possibly a state mismatch issue */
        err.error = 'State verification failed';
        err.error_description =
          'Unexpected error - You could face this issue if the server is not running with a correct `HASURA_GRAPHQL_PRO_KEY` ';
      }
      this.verificationError(err);
    }
  }
  verificationError(err) {
    this.setState({
      isErrorAuthenticating: true,
      errorMsg: {
        ...err,
      },
    });
  }
  render() {
    const styles = require('./OAuthCallback.scss');
    const { error: err, error_description: description } = this.state.errorMsg;
    const getErrorElement = () => {
      const getErrorCode = err ? (
        <div className={styles.error_code}>
          <FaExclamationCircle /> {err}
        </div>
      ) : null;
      const getErrorDescription = description ? (
        <div className={styles.error_description}>{description}</div>
      ) : null;
      const getBackButton =
        getErrorCode || getErrorDescription ? (
          <div className={styles.go_back_button}>
            <Link to="/login">
              <Button type="button" mode="primary" className="form-control">
                Login
              </Button>
            </Link>
          </div>
        ) : null;
      return (
        <div className={styles.error_wrapper}>
          {getErrorCode}
          {getErrorDescription}
          {getBackButton}
        </div>
      );
    };
    const getBody = () => {
      if (!this.state.errorMsg.error) {
        return (
          <div className={styles.validating_wrapper}>
            <div className={styles.steps}>Validating...</div>
          </div>
        );
      }
      return null;
    };
    return (
      <LoadingScreen isError={this.state.errorMsg.error}>
        {getBody()}
        {getErrorElement()}
      </LoadingScreen>
    );
  }
}

OAuthCallback.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const generatedCallbackConnector = connect => {
  return connect()(OAuthCallback);
};

export default generatedCallbackConnector;
