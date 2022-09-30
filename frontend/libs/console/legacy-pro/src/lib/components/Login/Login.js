import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { FaSpinner } from 'react-icons/fa';
import globals from '../../Globals';
import {
  loginClicked,
  UPDATE_ADMIN_SECRET_INPUT,
  UPDATE_PERSONAL_ACCESS_TOKEN,
  patLoginClicked,
  UPDATE_PROJECT_ID,
  UPDATE_PROJECT_NAME,
} from '../Main/Actions';
import { IconTooltip } from '@hasura/console-oss';

// import { FT_LOGIN_WITH_HASURA } from '../../helpers/versionUtils';

import { getFromLS, initLS } from './localStorage';

import LoginWith from './LoginWith';

import { isClientSet } from './utils';

const loginIcon = require('./login.svg');

class Login extends Component {
  constructor(props) {
    super(props);
    if (globals.consoleMode === 'cli') {
      this.state = { loginMethod: 'access-token' };
    } else {
      this.state = { loginMethod: 'admin-secret' };
    }
  }

  componentDidMount() {
    const currentState = getFromLS();
    if (!currentState) {
      initLS();
    }
  }

  handleAdminSecret = e => {
    this.props.dispatch({
      type: UPDATE_ADMIN_SECRET_INPUT,
      data: e.target.value,
    });
  };

  handlePAT = e => {
    this.props.dispatch({
      type: UPDATE_PERSONAL_ACCESS_TOKEN,
      data: e.target.value,
    });
    this.props.dispatch({
      type: UPDATE_PROJECT_ID,
      data: globals.projectId,
    });
    this.props.dispatch({
      type: UPDATE_PROJECT_NAME,
      data: globals.projectName,
    });
  };

  render() {
    const {
      loginInProgress,
      dispatch,
      // featuresCompatibility,
      location,
    } = this.props;

    // const { [FT_LOGIN_WITH_HASURA]: isLoginWithHasura } = featuresCompatibility;

    const styles = require('./Login.scss');
    const hasuraLogo = require('./black-logo.svg');
    const dropdownOptions = [
      { title: 'Admin Secret', value: 'admin-secret' },
      { title: 'Personal Access Token', value: 'access-token' },
    ];

    const renderLogin = () => {
      // Dont show "login with lux control plane" when running in pro-lite mode 
      if (isClientSet() && globals.consoleType !== 'pro-lite') {
        return (
          <LoginWith location={location}>
            <div
              className={`${styles.cursorPointer} ${styles.signin_btn} ${styles.display_flex}`}
            >
              Or login with your Hasura Cloud account
            </div>
          </LoginWith>
        );
        // return <LoginWith location={location} />;
      }
    };

    const handleLoginClick = e => {
      e.preventDefault();
      if (this.state.loginMethod === 'admin-secret') {
        dispatch(loginClicked());
      }
      if (this.state.loginMethod === 'access-token') {
        dispatch(patLoginClicked());
      }
    };

    const handleLoginInProgress = () => {
      if (loginInProgress) {
        return <FaSpinner className={styles.loginLoader} />;
      }
      return (
        <img
          className={styles.loginIcon}
          src={loginIcon}
          alt={'login icon'}
          onClick={handleLoginClick}
        />
      );
    };

    const renderPlaceholder = () => {
      if (this.state.loginMethod === 'admin-secret') {
        return `Enter ${globals.adminSecretLabel}`;
      }
      return `Enter ${globals.patLabel}`;
    };

    const handleInputChange = e => {
      if (this.state.loginMethod === 'admin-secret') {
        this.handleAdminSecret(e);
      }
      if (this.state.loginMethod === 'access-token') {
        this.handlePAT(e);
      }
    };

    const renderInput = () => {
      return (
        <div className={'input-group ' + styles.input_group}>
          <input
            onChange={handleInputChange}
            className={styles.form_input + ' form-control'}
            type="password"
            placeholder={renderPlaceholder()}
            name="password"
            aria-describedby="basic-addon2"
          />
          <span
            className={'input-group-addon ' + styles.input_addon_group}
            id="basic-addon2"
          >
            {handleLoginInProgress()}
          </span>
        </div>
      );
    };

    const renderDropdownOptions = () => {
      return dropdownOptions.map(option => {
        if (option.value === 'access-token' && globals.pro !== true) {
          return null;
        }
        return (
          <option
            key={option.value}
            className={styles.form_input + ' form-control'}
            value={option.value}
          >
            {option.title}
          </option>
        );
      });
    };

    const renderTitle = () => {
      if (globals.consoleMode === 'server') {
        return (
          <div
            className={`${styles.display_flex} ${styles.add_mar_bottom_small} relative z-10`}
          >
            Enter your admin secret
            <IconTooltip
              message="Admin secret is the secret key to access your GraphQL API in admin mode. If you own this project, you can find the admin secret on the projects dashboard."
            />
          </div>
        );
      }
      return (
        <div
          className={`input-group ${styles.input_group} ${styles.add_mar_bottom_mid}`}
        >
          <select
            className={styles.form_input + ' form-control'}
            value={this.state.loginMethod}
            onChange={e => {
              this.setState({ loginMethod: e.target.value });
            }}
            placeholder="Select Login Method"
          >
            {renderDropdownOptions()}
          </select>
        </div>
      );
    };

    return (
      <div className={styles.mainWrapper + ' container-fluid'}>
        <div className={styles.container + ' container'} id="login">
          <div className={styles.loginCenter}>
            <Helmet title={'Login | ' + 'Hasura'} />
            <div className={styles.hasuraLogo}>
              <img src={hasuraLogo} />
            </div>
            <div className={styles.loginWrapper}>
              <form className="form-horizontal" onSubmit={handleLoginClick}>
                <div
                  className={styles.input_addon_group + ' ' + styles.padd_top}
                >
                  {renderTitle()}
                  {renderInput()}
                </div>
                {renderLogin()}
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
  const mapStateToProps = (state, ownProps) => {
    return {
      location: ownProps.location,
      loginInProgress: state.main.loginInProgress,
      adminSecretError: state.tables.adminSecretError,
      featuresCompatibility: state.main.featuresCompatibility,
    };
  };
  return connect(mapStateToProps)(Login);
};

export default generatedLoginConnector;
