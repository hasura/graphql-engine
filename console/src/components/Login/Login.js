import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';

import { loginClicked, UPDATE_ACCESS_KEY_INPUT } from '../Main/Actions';

class Login extends Component {
  handleAccessKey = e => {
    this.props.dispatch({
      type: UPDATE_ACCESS_KEY_INPUT,
      data: e.target.value,
    });
  };
  loginClicked = () => {
    this.props.dispatch(loginClicked());
  };
  render() {
    const { loginInProgress, loginError } = this.props;
    let loginText = 'Enter';
    const styles = require('./Styles.scss');
    if (loginInProgress) {
      loginText = (
        <span>
          Verifying...
          <i className="fa fa-spinner fa-spin" aria-hidden="true" />
        </span>
      );
    } else if (loginError) {
      loginText = 'Error. Try again?';
    }

    const hasuraLogo = require('./blue-logo.svg');
    const { dispatch } = this.props;
    return (
      <div className={styles.mainWrapper + ' container-fluid'}>
        <div className={styles.container + ' container'} id="login">
          <div className={styles.loginCenter}>
            <Helmet title={'Login | ' + 'Hasura'} />
            <div className={styles.hasuraLogo}>
              <img src={hasuraLogo} />
            </div>
            <div className={styles.loginWrapper}>
              <form
                className="form-horizontal"
                onSubmit={e => {
                  e.preventDefault();
                  dispatch(loginClicked());
                }}
              >
                <div
                  className={styles.input_addon_group + ' ' + styles.padd_top}
                >
                  <div className={'input-group ' + styles.input_group}>
                    <input
                      onChange={this.handleAccessKey}
                      className={styles.form_input + ' form-control'}
                      placeholder="Password"
                      type="password"
                      placeholder="Enter access key"
                      name="password"
                    />
                  </div>
                </div>
                <div className={styles.signin_btn}>
                  <button type="submit" className="form-control">
                    {loginText}
                  </button>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

Login.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const generatedLoginConnector = connect => {
  const mapStateToProps = state => {
    return {
      loginInProgress: state.main.loginInProgress,
      loginError: state.main.loginError,
      accessKeyError: state.tables.accessKeyError,
    };
  };
  return connect(mapStateToProps)(Login);
};

export default generatedLoginConnector;
