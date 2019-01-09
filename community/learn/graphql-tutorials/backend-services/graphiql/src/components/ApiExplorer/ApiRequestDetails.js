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
    console.log(this.props);
    const { isAuthenticated } = this.props.auth;
    if (!isAuthenticated()) {
      // window.location.href = '/graphiql';
    }
    const styles = require('./ApiExplorer.scss');
    return (
      <div className={styles.apiRequestWrapper + ' ' + styles.apiContentPadd}>
        <div className={styles.apiRequestContent}>{this.props.description}</div>
        {!isAuthenticated() && (
          <button
            id="qsLoginBtn"
            className={styles.authBtn + ' ' + styles.authBtnLogin + ' btn'}
            onClick={this.login.bind(this)}
          >
            Log In
          </button>
        )}
        {isAuthenticated() && (
          <button
            id="qsLogoutBtn"
            className={styles.authBtn + ' btn btn-danger'}
            onClick={this.logout.bind(this)}
          >
            Log Out
          </button>
        )}
      </div>
    );
  }
}

ApiRequestDetails.propTypes = {
  title: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default ApiRequestDetails;
