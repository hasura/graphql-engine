import React, { Component } from 'react';
import PropTypes from 'prop-types';

class ApiRequestDetails extends Component {
  login() {
    this.props.auth.login();
  }

  logout() {
    this.props.auth.logout();
  }

  render() {
    const { isAuthenticated } = this.props.auth;

    const styles = require('./ApiExplorer.scss');

    const loginButton = (
      <button
        id="qsLoginBtn"
        className={'btn btn-primary'}
        onClick={this.login.bind(this)}
      >
        Log In
      </button>
    );

    const logoutButton = (
      <button
        id="qsLogoutBtn"
        className={'btn btn-danger'}
        onClick={this.logout.bind(this)}
      >
        Log Out
      </button>
    );

    const loginOverlay = (
      <div className={styles.overlay}>
        <div className={styles.overlayContent}>
          <div className={styles.overlayHeading}>
            Welcome to the GraphQL tutorial GraphiQL
          </div>
          <div className={styles.overlayMessage}>
            Please login to continue
          </div>
          <div className={styles.overlayAction}>
            { loginButton }
          </div>
        </div>
      </div>
    );

    return (
      <div>
        { isAuthenticated() || loginOverlay }

        <div className={styles.apiRequestWrapper + ' ' + styles.apiContentPadd}>
          <div className={styles.apiRequestContent}>{this.props.description}</div>

          <div className={styles.authBtnWrapper}>
            { isAuthenticated() ? logoutButton : loginButton }
          </div>

        </div>
      </div>
    );
  }
}

ApiRequestDetails.propTypes = {
  title: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default ApiRequestDetails;
